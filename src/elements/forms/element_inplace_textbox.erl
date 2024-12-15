% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_inplace_textbox).
-include("wf.hrl").
-export([
    reflect/0,
    transform_element/1,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, inplace_textbox).

-spec transform_element(#inplace_textbox{}) -> body().
transform_element(Record) -> 
    % Get vars...
    [OKButtonID, CancelButtonID,
    ViewPanelID, EditPanelID,
    LabelID, MouseOverID, TextBoxID] = wf:temp_ids(7),

    Tag = Record#inplace_textbox.tag,
    Delegate = Record#inplace_textbox.delegate,
    StartMode = Record#inplace_textbox.start_mode,
    HoverText = wf:coalesce([Record#inplace_textbox.hover_text, <<"Click to edit">>]),

    % Set up the events...
    Controls = {ViewPanelID, LabelID, EditPanelID, TextBoxID},
    OKPostback = {ok, Delegate, Controls, Tag},
    %KEvent = #event{delegate=?MODULE, postback= },

    % Create the view...
    Text = Record#inplace_textbox.text,

    ViewActions = [
        #event{type=click, actions=[
            #hide{target=ViewPanelID},
            #show{target=EditPanelID},
            wf:f("let e = obj('~s'); e.focus(); e.select();", [TextBoxID])
        ]},
        #event{type=mouseover, target=MouseOverID, actions=#show{}},
        #event{type=mouseout, target=MouseOverID, actions=#hide{}}
    ],

    CancelButtonClick = [
        #hide{target=EditPanelID},
        #show{target=ViewPanelID},
        wf:f("let e = obj('~s'); e.value=e.defaultValue;",[TextBoxID])
    ],

    {ViewPanelStyle, EditPanelStyle, StartModeActions} = start_mode_style(TextBoxID, StartMode),

    wf:defer(StartModeActions),

    Terms = #panel { 
        id=Record#inplace_textbox.id,
        html_id=Record#inplace_textbox.html_id,
        class=[inplace_textbox, Record#inplace_textbox.class],
        title=Record#inplace_textbox.title,
        data_fields=Record#inplace_textbox.data_fields,
        aria=Record#inplace_textbox.aria,
        style=Record#inplace_textbox.style,
        body = [
            #panel{id=ViewPanelID, class=view, actions=ViewActions, style=ViewPanelStyle, body=[
                #span{id=LabelID, class=label, text=Text, html_encode=Record#inplace_textbox.html_encode, actions=[
                    #buttonize{target=ViewPanelID}
                ]},
                #span{id=MouseOverID, class=instructions, text=HoverText, style="display:none"}
            ]},
            #panel{id=EditPanelID, class=edit, style=EditPanelStyle, body=[
                #textbox{id=TextBoxID, class=textbox, text=Text, delegate=?MODULE, postback=OKPostback, next=OKButtonID},
                #button{id=OKButtonID, class=inplace_ok, text="OK", delegate=?MODULE, postback=OKPostback},
                #button{id=CancelButtonID, class=inplace_cancel, text="Cancel", click=CancelButtonClick}
            ]}
        ]
    },

    %case Record#inplace_textbox.start_mode of
    %    view -> wf:wire(EditPanelID, #hide{});
    %    edit -> 
    %end,

    %wf:wire(OKButtonID, OKEvent#event{type=click}),
            
    %% OkButton will trigger it
    %% TextBoxID is what's being validated
    %% CancelButtonID is where the error will show
    wf:defer(OKButtonID, TextBoxID, #validate{attach_to=CancelButtonID, validators=Record#inplace_textbox.validators}),
    wf:defer(TextBoxID, TextBoxID, #validate{attach_to=CancelButtonID, validators=Record#inplace_textbox.validators}),

    Terms.

-spec start_mode_style(TextboxID :: id(), view | edit) -> {ViewStyle :: string(), EditStyle :: string(), OtherActions :: actions()}.
start_mode_style(_TextboxID, view) ->
    {"", "display:none", ""};
start_mode_style(TextboxID, edit) ->
    Actions = wf:f("let e = obj('~s'); e.focus(); e.select();", [TextboxID]),
    {"display:none", "", Actions}.

-spec event(any()) -> ok.
event({ok, Delegate, {ViewPanelID, LabelID, EditPanelID, TextBoxID}, Tag}) -> 
    Value = wf:q(TextBoxID),
    Module = wf:coalesce([Delegate, wf:page_module()]),
    Value1 = Module:inplace_textbox_event(Tag, Value),
    wf:update(LabelID, Value1),
    wf:set(TextBoxID, Value1),
    wf:wire(EditPanelID, #hide{}),
    wf:wire(ViewPanelID, #show{}),
    wf:wire(wf:f("obj('~s').defaultValue = '~s';",[TextBoxID,wf:js_escape(Value1)])),
    ok;

event(_Tag) -> ok.
