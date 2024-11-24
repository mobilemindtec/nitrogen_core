% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module(default_validation_handler).
-behaviour(validation_handler).
-include("wf.hrl").
-export([
    init/2,
    finish/2,
%    attach/5,
%    validate/4,
    js_constructor/7,
    js_add_validator/6,
    required_js/2
%    required_css/2
]).


init(_Config, State) ->
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

%validate(Field, Value, _Config, State) ->
%    true.
%
%attach(Targetid, Field, Validators, _Config, State) ->
%    State.

-define(JS_SCRIPT, <<"/nitrogen/nitro_validation.js">>).

required_js(undefined, _State) ->
    ?JS_SCRIPT;
required_js(Config, _State) ->
    ds:get(Config, required_js, ?JS_SCRIPT).

%required_css(Config, _State) ->
%    ds:get(Config, required_css, undefined).
%

js_constructor(TargetPath, ValidationGroup, ValidMessage, On, AttachTo, Config, State) ->
    %ValidMessage = wf:js_escape(Record#validate.success_text),
    OnlyOnBlur = lists:member(blur, On),
    OnlyOnSubmit = lists:member(submit, On),
    InsertAfterNode = case AttachTo of
        undefined -> "";
        Node -> wf:f(<<", attachTo: obj(\"~s\")">>, [Node])
    end,
    % Create the validator Javascript...
    
    ConstructorJS = wf:f(<<"var v = Nitrogen.$init_validation(obj('~s'), '~s', { validMessage: \"~ts\", onlyOnBlur: ~s, onlyOnSubmit: ~s ~s});">>,
                         [TargetPath, ValidationGroup, wf:js_escape(ValidMessage), OnlyOnBlur, OnlyOnSubmit, InsertAfterNode]),
    
    CombinedJS = [
        ConstructorJS
    ],
    {NewState, NewScript} = maybe_dependency_wrap(CombinedJS, Config, State),
    {ok, NewScript, NewState}.



%% NOTE: This needs to be reworked to be put into the validation_handler
%% module, but in order to do so, it'll need toe rely on the normal wf:state
%% instead of its internal state - UNLESS wf_core is modified to add the
%% validation_handler to the list of handlers that are serialized and
%% deserialized
 maybe_dependency_wrap(Script, Config, State) ->
    case ds:get(State, validation_prewrapped, false) of
        true ->
            {State, Script};
        false ->
            JS = required_js(Config, State),
            NewState = ds:set(State, validation_prewrapped, true),
            NewScript = [
                #script{
                    dependency_js=JS,
                    script=#console_log{text="JS is loaded: " ++ JS}
                },
                Script
            ],
            {NewState, NewScript}
    end.

   
%% Maybe rename ValidationGroup to Trigger?

-spec js_add_validator(Target :: id(), validator_type(), FM :: text(), Opts :: proplist() | map(), Config :: any(), State :: any()) -> text().
js_add_validator(Target, Type, FM, Opts, Config, State) ->
    case js_add_validator_inner(Type, FM, Opts, Config, State) of
        X when ?WF_BLANK(X) ->
            "";
        FunBody ->
            [
                %% Nitrogen.$add_validation(target, update_v_fun)
                %% update_v_fun is a function of arity 1 were the only argument
                %% is the validator stored in the element's data tag
                wf:f(<<"Nitrogen.$add_validation('~s', ">>, [wf:js_escape(Target)]),
                    <<"\nfunction(v) {">>,
                        FunBody,
                    <<"});">>
            ]
    end.

%% FM=FailureMessage
%% For now, we're not implementing these javascript elements 
js_add_validator_inner(integer, _FM, _Opts, _Config, _State) ->
    [];
js_add_validator_inner(number, _FM, _Opts, _Config, _State) ->
    [];
js_add_validator_inner(not_blank, _FM, _Opts, _Config, _State) ->
    [];
js_add_validator_inner(email, _FM, _Opts, _Config, _State) ->
    [];
js_add_validator_inner(max_length, _FM, _Opts, _Config, _State) ->
    [];
js_add_validator_inner(min_length, _FM, _Opts, _Config, _State) ->
    [];
js_add_validator_inner(custom, FM, Opts, _Config, _State) ->
    [Function, Args, WhenEmpty0] = ds:get_list(Opts, [function, args, when_empty]),
    WhenEmpty = wf:to_bool(WhenEmpty0),
    Fun = [
        <<"\nfunction(v) { ">>,
            <<"\nreturn ">>,action_js_fun:render_action(#js_fun{function=Function, args=Args}),
        <<"}">>
    ],
    ValOpts = wf:json_encode([
        {msg, wf:to_unicode_binary(FM)},
        {when_empty, WhenEmpty}
    ]),
    %% v here is the validator stored in the data tag.
    %% v.add(validation_fun, opts)
    [<<"v.add(">>, Fun, ",", ValOpts, <<");">>];
js_add_validator_inner(_, _, _, _, _) ->
    [].
