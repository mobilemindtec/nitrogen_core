% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render_elements).
-include("wf.hrl").
-export ([
    render_elements/1,
    render_and_trap_actions/1,
    temp_id/0,
    normalize_id/1,
    recurse_body/2,
    recurse_body_like/2,
    recurse_fields/3,
    body_like_fields/0,
    cache_rendered/3
]).

-spec render_and_trap_actions(Elements :: body() | fun()) -> {ok, Html :: binary(), Actions :: binary()}.
render_and_trap_actions(Elements) ->
    ?WF_IF(not(wf:in_request()), wf_context:init_context(undefined)),
    OldActionQueue = wf_context:action_queue(),
    wf_context:clear_action_queue(),
    {ok, Html} = case is_function(Elements) of
        true -> render_elements(Elements());
        false -> render_elements(Elements)
    end,
    {ok, JS} = wf_render_actions:render_action_queue(),
    wf_context:action_queue(OldActionQueue),
    {ok, Html, wf:to_unicode_binary(JS)}.

cache_rendered(Key, TTL, Fun) ->
    {ok, Html, Actions} = wf:cache(Key, TTL, fun() ->
        wf:render_isolated(Fun)
    end),
    wf:wire(Actions),
    Html.

% Render elements and return the HTML that was produced.
% Puts any new actions into the current context.
-spec render_elements(Elements :: body()) -> {ok, html()}.
render_elements(Elements) ->
    %wf_utils:write_debug(render_elements, Elements),
    {ok, inner_render_elements(Elements)}.

-spec inner_render_elements(E :: body()) -> html().
inner_render_elements(undefined) ->
    [];
inner_render_elements([]) ->
    [];
inner_render_elements(E) when is_binary(E); is_integer(E) ->
    E;
inner_render_elements([E|T]) ->
    [inner_render_elements(E) | inner_render_elements(T)];
inner_render_elements(E) when is_tuple(E) ->
    %wf_utils:write_debug(render_element, E),
    render_element(E);
inner_render_elements(mobile_script) ->
    mobile_script;
inner_render_elements(script) ->
    script;
inner_render_elements(Atom) when is_atom(Atom) ->
    wf:to_binary(Atom);
inner_render_elements(Unknown) ->
    throw({unanticipated_case_in_render_elements, Unknown}).

% This is a Nitrogen element, so render it.
-spec render_element(nitrogen_element()) -> html().
render_element(Element) when is_tuple(Element) ->
    % Get the element's backing module...
    Base = wf_utils:get_elementbase(Element),
    verify_and_render(Base, Element).

verify_and_render(Base = #elementbase{is_element=is_element}, Element) ->
    inner_render_element(Base, Element);
verify_and_render(_, Element) ->
    wf:error("Attempting to render an element that is not an element: ~p",[Element]),
    wf:f("Unrenderable Element: <pre>~p</pre>",[Element]).
    %throw({not_an_element, Element}).

-spec inner_render_element(#elementbase{}, nitrogen_element()) -> html().
inner_render_element(#elementbase{show_if=false}, _Element) ->
    [];
inner_render_element(Base = #elementbase{show_if=true}, Element) ->
    Module = Base#elementbase.module, 
    case code:ensure_loaded(Module) of
        {module, Module} ->
            prepare_and_render_or_transform(Module, Base, Element);
        Response ->
            ElementName = element(1, Element),
            exit({failed_to_ensure_module_loaded, [{element, ElementName}, {'response_from code:ensure_loaded', Response}, {module, Module}, {record, Element}]})
    end.

-spec prepare_and_render_or_transform(Module :: atom(), #elementbase{}, nitrogen_element()) -> html().
prepare_and_render_or_transform(Module, Base, Element) ->
    case erlang:function_exported(Module,transform_element,1) of
        true ->
            %% Module:transform_element is a special shortcut mechanism
            %% of rendering elements without any of the overhead of
            %% the pre-rendering that goes on with each element. This
            %% should be used for custom elements that are simply
            %% defined in terms of other Nitrogen elements.
            _Html = call_element_render(transform_element, Module, Element);
        false ->
            prepare_and_render(Module, Base, Element)
    end.

-spec prepare_and_render(Module :: atom(), #elementbase{}, nitrogen_element()) -> html().
prepare_and_render(Module, Base, Element) ->
    % TODO: Revisit generating an ID for each element, instead
    % generating the ID only if an element has actions.
    % Otherwise, if an element needs an ID for something
    % special (like how the #button element needs an anchor to
    % wire the postback to and for validation stuff), let the
    % element_render function take care of that itself.
    %
    % This change will provide a handful of performance
    % improvements:
    %   + Removes having to call temp_id() for every element
    %   + Removes having to call normalize_id() possibly twice
    %     for each element
    %   + Lightens the page size since every element won't have
    %     an unnecessary 'tempABCXYZ' class.
        
    % Get the anchor, ID, and Class, or create a new ones if not defined...
    Anchor = extract_anchor(Base),
    ID = extract_id(Base, Anchor),
    Class = extract_class(Base, Anchor, ID),

    % Update the base element with the new id and class...
    Base1 = Base#elementbase{id=ID, anchor=Anchor, class=Class},
    Element1 = wf_utils:replace_with_base(Base1, Element),

    % Wire the actions...           
    wf_context:anchor(Anchor),
    wf:wire(Base1#elementbase.actions),

    % Render the element...
    Html = call_element_render(render_element, Module, Element1),

    % Reset the anchor (likely changed during the inner render)...
    wf_context:anchor(Anchor),
    Html.

extract_anchor(#elementbase{anchor=undefined}) ->
    normalize_id(temp_id());
extract_anchor(#elementbase{anchor=Anchor}) ->
    normalize_id(Anchor).

% Get the ID, or use the anchor if it's not defined...
extract_id(#elementbase{id=undefined}, Anchor) ->
    Anchor;
extract_id(#elementbase{id=ID}, _Anchor) ->
    normalize_id(ID).

extract_class(#elementbase{class=Class}, ID, Anchor) when ID==Anchor ->
    [ID, Class];
extract_class(#elementbase{class=Class}, ID, Anchor) ->
    [ID, Anchor, Class].


% call_element_render(RenderOrTransform, Module, Element) -> {ok, Html}.  Calls
% the render_element/3 function of an element to turn an element record into
% HTML.
-spec call_element_render(RenderOrTransform :: render_element | transform_element,
                          Module :: module(),
                          Element :: nitrogen_element() ) -> html().
call_element_render(RenderOrTransform, Module, Element) ->
    NewElements = Module:RenderOrTransform(Element),
    inner_render_elements(NewElements).

-spec normalize_id(list() | atom() | binary()) -> string().
normalize_id(ID) -> 
    case wf:to_string_list(ID) of
        [".wfid_" ++ _] = [NormalizedID] -> NormalizedID;
        ["page"] -> "page";
        [NewID]  -> ".wfid_" ++ NewID
    end.

-spec temp_id() -> string().
temp_id() ->
    Num = ?WF_UNIQUE,  %% For Erlang 18+, is erlang:unique_integer,
                       %% For Erlang <18, is parts of erlang:now()
                       %% see compat.escript, and include/compat.hrl for the
                       %% definition.
    "temp" ++ integer_to_list(Num).

recurse_body(Fun, Elements) ->
    recurse_fields([body], Fun, Elements).

body_like_fields() ->
    %% These are just some semi-common record fields that should be recursed
    %% when searching for body elements.
    [body, empty_body, rows, cells].

recurse_body_like(Fun, Elements) ->
    recurse_fields(body_like_fields(), Fun, Elements).

recurse_fields(BFields, Fun, List) when is_list(List) ->
    [recurse_fields(BFields, Fun, X) || X <- List];
recurse_fields(BFields, Fun, Rec0) when ?IS_ELEMENT(Rec0) ->
    try
        %% First, apply our Fun to the record
        Rec = Fun(Rec0),

        %% To get the fields in this element, we need to know its module to call `reflect()`
        %% Because all Nitrogen elements start with #elementbase, we can get
        %% its module field with #elementbase.module
        Mod = element(#elementbase.module, Rec),
        Fields = Mod:reflect(),

        %% Iterate through the "BodyFields" to recurse into those fields
        lists:foldl(fun(BodyField, Acc) ->
            case wf_utils:indexof(BodyField, Fields) of
                undefined ->
                    Acc;
                Idx ->
                    Body = element(Idx, Acc),
                    Body2 = recurse_fields(BFields, Fun, Body),
                    setelement(Idx, Acc, Body2)
            end
        end, Rec, BFields)
    catch E:T:S ->
        error_logger:warning_msg("Error trying to apply function to an element tuple: ~p~nError: ~p: ~p.~nStacktrace: ~p",[Rec0, E, T, S]),
        Rec0
    end;
recurse_fields(_BFields, _Fun, X) ->
    X.



    
    
