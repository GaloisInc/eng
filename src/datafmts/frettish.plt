:- begin_tests(frettish).
:- use_module("frettish").
:- use_module(library(strings)).

test(state_change, [nondet]) :-
    Inp = "frog shall always satisfy if (wet & awake) then (noise = croaking)",
    parse_fret("test", Inp,
               fretment(scope_info(SFret, SVars),
                                  condition_info(CFret, CVars),
                                  component_info(Comp),
                                  timing_info(Timing, TVars),
                                  response_info(Responses, RespVars))),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == null),
    assertion(SVars == []),
    get_dict(condition, CFret, Condition),
    assertion(Condition == "null"),
    assertion(CVars == []),
    get_dict(component_name, Comp, CompName),
    assertion(CompName == "frog"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "always"),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond == "(((wet & awake)) => ((noise = croaking)))"),
    assertion(RespVars == [ "wet", "awake", "noise", "croaking" ]).

test(stage_change_natural, [nondet]) :-
    Inp = "Upon wet & awake the frog shall always satisfy (noise = croaking)",
    parse_fret("test", Inp,
               fretment(scope_info(SFret, SVars),
                                  condition_info(CFret, CVars),
                                  component_info(Comp),
                                  timing_info(Timing, TVars),
                                  response_info(Responses, RespVars))),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == null),
    assertion(SVars == []),
    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "regular"),
    assertion(Qualifier == "upon"),
    assertion(PreCond == "(wet & awake)"),
    assertion(RegCond == "(wet & awake)"),
    assertion(CVars == ["wet", "awake"]),
    get_dict(component_name, Comp, CompName),
    assertion(CompName == "frog"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "always"),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond == "((noise = croaking))"),
    assertion(RespVars == [ "noise", "croaking" ]).

test(stage_change_scope, [nondet]) :-
    Inp = "In awake mode upon wet the frog shall always satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "in").

test(stage_change_scope_during, [nondet]) :-
    Inp = "During awake mode upon wet the frog shall always satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "in").

test(stage_change_scope_in, [nondet]) :-
    Inp = "Unless in awake mode upon wet the frog shall always satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "notin").

test(stage_change_scope_after, [nondet]) :-
    Inp = "After awake mode upon wet the frog shall always satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "after", false, false).

test(stage_change_scope_before, [nondet]) :-
    Inp = "Before awake mode upon wet the frog shall always satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "before", false, false).

test(stage_change_scope_before_cond, [nondet]) :-
    Inp = "Before awake upon wet the frog shall always satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "before", false, false).

% TODO scope only ...
% TODO scope except ...

check_scs_scope(FretMent, ScopeType, Exclusive, Required) :-
    check_scs_scope(FretMent, ScopeType),
    FretMent = fretment(scope_info(SFret, _SVars),
                        condition_info(_CFret, _CVars),
                        component_info(_Comp),
                        timing_info(_Timing, _TVars),
                        response_info(_Responses, _RespVars)),
    get_dict(exclusive, SFret, Excl),
    assertion(Excl == Exclusive),
    get_dict(required, SFret, Rqrd),
    assertion(Rqrd == Required).

check_scs_scope(FretMent, ScopeType) :-
    FretMent = fretment(scope_info(SFret, SVars),
                        condition_info(CFret, CVars),
                        component_info(Comp),
                        timing_info(Timing, TVars),
                        response_info(Responses, RespVars)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == ScopeType),
    get_dict(scope_mode, SFret, SMode),
    assertion(SMode == "awake"),
    assertion(SVars == ["awake"]),
    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "regular"),
    assertion(Qualifier == "upon"),
    assertion(PreCond == "(wet)"),
    assertion(RegCond == "(wet)"),
    assertion(CVars == ["wet"]),
    get_dict(component_name, Comp, CompName),
    assertion(CompName == "frog"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "always"),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond == "((noise = croaking))"),
    assertion(RespVars == [ "noise", "croaking" ]).

test(stage_change_scope_invert, [nondet]) :-
    Inp = "When not in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "never").

test(stage_change_scope_invert_if, [nondet]) :-
    Inp = "If not in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "never").

test(stage_change_scope_invert_unless, [nondet]) :-
    Inp = "unless in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "never").

check_scsi_with_timing(FretMent, WantedTiming) :-
    check_scsi_with_timing_common(FretMent, Timing, []),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == WantedTiming).

check_scsi_with_timing(FretMent, WantedTiming, Duration) :-
    check_scsi_with_timing_common(FretMent, Timing, []),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == WantedTiming),
    get_dict(duration, Timing, Dur),
    assertion(Dur == Duration).

check_scsi_with_timing(FretMent, WantedTiming, StopCond, TimingVars) :-
    check_scsi_with_timing_common(FretMent, Timing, TimingVars),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == WantedTiming),
    get_dict(stop_condition, Timing, SC),
    assertion(SC == StopCond).

check_scsi_with_timing_common(FretMent, Timing, TimingVars) :-
    FretMent = fretment(scope_info(SFret, SVars),
                        condition_info(CFret, CVars),
                        component_info(Comp),
                        timing_info(Timing, TVars),
                        response_info(Responses, RespVars)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == "notin"),
    get_dict(scope_mode, SFret, SMode),
    assertion(SMode == "awake"),
    assertion(SVars == ["awake"]),

    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "noTrigger"),
    assertion(Qualifier == "whenever"),
    assertion(PreCond == "(wet)"),
    assertion(RegCond == "(wet)"),
    assertion(CVars == ["wet"]),

    get_dict(component_name, Comp, CompName),
    assertion(CompName == "frog"),

    assertion(TVars == TimingVars),

    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond == "((noise = croaking))"),
    assertion(RespVars == [ "noise", "croaking" ]).

test(stage_change_scope_invert_eventually, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall eventually satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "eventually").

test(stage_change_scope_invert_finally, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall finally satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "finally").

test(stage_change_scope_invert_at_the_last_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the last timepoint satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "finally"). % n.b. at the last timepoint = finally

test(stage_change_scope_invert_at_the_same_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the same timepoint satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately"). % n.b. at the same timepoint = immediately

test(stage_change_scope_invert_at_the_first_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the first timepoint satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately"). % n.b. at the first timepoint = immediately

test(stage_change_scope_invert_initially, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall initially satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately"). % n.b. initially = immediately

test(stage_change_scope_invert_immediately, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall immediately satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately").

test(stage_change_scope_invert_at_the_next_timepoint, [nondet, blocked(true)]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the next timepoint satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "next").

test(stage_change_scope_invert_within_3s, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall within 3 seconds satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "within", "3 ").

test(stage_change_scope_invert_for_3s, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall for 3 seconds satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "for", "3 ").

test(stage_change_scope_invert_after_3s, [nondet, blocked(true)]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall after 3 seconds satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "after", "3").
    % KWQ: unrealizable?

test(stage_change_scope_invert_until, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall until !wet satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "until", "(! wet)", ["wet"]).

test(stage_change_scope_invert_before, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall before !wet satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "before", "(! wet)", ["wet"]).

%% capture above 3 in FRET, then validate frettish can parse.  Then return to scope.  And finally condition exprs.

test(with_ltl_predicate, [nondet]) :-
    Inp = "whenever awake & persisted(2, wet) the frog shall at the next timepoint satisfy (noise = croaking)",
    parse_fret("test", Inp, FretMent),
    FretMent = fretment(scope_info(SFret, SVars),
                        condition_info(CFret, CVars),
                        component_info(Comp),
                        timing_info(Timing, TVars),
                        response_info(Responses, RespVars)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == null),
    assertion(SVars == []),
    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "noTrigger"),
    assertion(Qualifier == "whenever"),
    assertion(PreCond == "(awake & persisted(2, wet))"),
    assertion(RegCond == "(awake & persisted(2, wet))"),
    assertion(CVars == ["awake", "wet"]),
    get_dict(component_name, Comp, CompName),
    assertion(CompName == "frog"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "next"),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond == "((noise = croaking))"),
    assertion(RespVars == [ "noise", "croaking" ]).

%% Then do system pre/post

%%                    CI
%%                    RACK
%% Actual specs!
