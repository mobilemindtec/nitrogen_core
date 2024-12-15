% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(action_clear_validation).
-include("wf.hrl").
-compile(export_all).

render_action(Record) -> 
    % Some values...
    TriggerPath = wf_render_actions:normalize_path(Record#clear_validation.validation_trigger),
    TargetPath = wf_render_actions:normalize_path(Record#clear_validation.validation_target),
	BothPath = wf_render_actions:normalize_path(Record#clear_validation.validation_all),

	if
		TriggerPath == undefined andalso TargetPath == undefined andalso BothPath == undefined ->
			%% nothing defined, so let's clear all validations
			clear_all_validators();
		BothPath =/= undefined ->
			%% If Let's just wipe out any validators at all related to the defined elements
			clear_trigger_validators(BothPath),
			clear_target_validators(BothPath);
		TriggerPath =/= undefined andalso TargetPath =/= undefined ->
			%% clear a validators on specific targets performed by specific triggers
			clear_specific_validators(TriggerPath, TargetPath);
		TriggerPath =/= undefined ->
			%% Clear validators triggered by a specific button
			clear_trigger_validators(TriggerPath);
		TargetPath =/= undefined ->
			%% Clear all validators assigned to specific fields
			clear_target_validators(TargetPath);
		true -> throw({action_clear_validation,neither_trigger_nor_target_defined})
	end.

clear_all_validators() ->
    wf_validation:clear_all_validators(),
	"Nitrogen.$destroy_all_validation()".

clear_specific_validators(Trigger, Target) ->
    wf_validation:clear_specific_validators(Trigger, Target),
	wf:f("Nitrogen.$destroy_specific_validation('~ts','~ts')",[Trigger, Target]).

clear_target_validators(Target) ->
    wf_validation:clear_target_validators(Target),
	wf:f("Nitrogen.$destroy_target_validation('~ts')",[Target]).

clear_trigger_validators(Trigger) ->
    wf_validation:clear_trigger_validators(Trigger),
	wf:f("Nitrogen.$destroy_validation_group('~ts')",[Trigger]).

