:- begin_tests(frettish).
:- use_module(frettish).
:- use_module('../englib').
:- use_module(library(strings)).

test(state_change, [nondet]) :-
    Inp = "frog shall always satisfy if (wet & awake) then (noise = croaking)",
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
    assertion(RespVars == [ "wet", "awake", "noise", "croaking" ]),
    emit_fretish(FretMent, Out),
    ExpOut = "the frog shall always satisfy (((wet & awake)) => ((noise = croaking))).",
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, Ranges),
    assertion(Out == Out2),
    assertion(Ranges == ranges{ conditionTextRange:[0, 0],
                                componentTextRange:[0, 7],
                                timingTextRange:[15, 20],
                                responseTextRange:[22, 70]
                              }).

test(state_change_noparen_expr, [nondet]) :-
    Inp = "frog shall always satisfy if (wet & awake) then noise = croaking",
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
    assertion(PostCond == "(((wet & awake)) => (noise = croaking))"),
    assertion(RespVars == [ "wet", "awake", "noise", "croaking" ]),
    emit_fretish(FretMent, Out),
    ExpOut = "the frog shall always satisfy (((wet & awake)) => (noise = croaking)).",
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, Ranges),
    assertion(Out == Out2),
    assertion(Ranges == ranges{ conditionTextRange:[0, 0],
                                componentTextRange:[0, 7],
                                timingTextRange:[15, 20],
                                responseTextRange:[22, 68]
                              }).

test(stage_change_natural, [nondet]) :-
    Inp = "Upon wet & awake the frog shall always satisfy (noise = croaking)",
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
    assertion(RespVars == [ "noise", "croaking" ]),
    emit_fretish(FretMent, Out),
    ExpOut = "upon (wet & awake) the frog shall always satisfy ((noise = croaking)).",
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, Ranges),
    assertion(Out == Out2),
    assertion(Ranges == ranges{ conditionTextRange:[0, 17],
                                componentTextRange:[19, 26],
                                timingTextRange:[34, 39],
                                responseTextRange:[41, 68]
                              }).

test(stage_change_scope, [nondet]) :-
    Inp = "In awake upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "in awake upon (wet) the frog shall always satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "in", ExpOut, ranges{ scopeTextRange:[0, 7],
                                                    conditionTextRange:[9, 18],
                                                    componentTextRange:[20, 27],
                                                    timingTextRange:[35, 40],
                                                    responseTextRange:[42, 69]
                                                  }).

test(expr_scope, [nondet]) :-
    Inp = "while mind = awake upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "in mind = awake upon (wet) the frog shall always satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "in", [ "mind", "awake" ], ExpOut,
                    ranges{ scopeTextRange:[0, 14],
                            conditionTextRange:[16, 25],
                            componentTextRange:[27, 34],
                            timingTextRange:[42, 47],
                            responseTextRange:[49, 76]
                          }).

test(stage_change_scope_during, [nondet]) :-
    Inp = "During awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "in awake upon (wet) the frog shall always satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "in", ExpOut,
                    ranges{ scopeTextRange:[0, 7],
                            conditionTextRange:[9, 18],
                            componentTextRange:[20, 27],
                            timingTextRange:[35, 40],
                            responseTextRange:[42, 69]
                          }).

test(stage_change_scope_in, [nondet]) :-
    Inp = "Unless in awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "unless in awake upon (wet) the frog shall always satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "notin", ExpOut,
                    ranges{ scopeTextRange:[0, 14],
                            conditionTextRange:[16, 25],
                            componentTextRange:[27, 34],
                            timingTextRange:[42, 47],
                            responseTextRange:[49, 76]
                          }).

test(stage_change_scope_after, [nondet]) :-
    Inp = "After awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "after awake upon (wet) the frog shall always satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "after", ExpOut,
                    ranges{ scopeTextRange:[0, 10],
                            conditionTextRange:[12, 21],
                            componentTextRange:[23, 30],
                            timingTextRange:[38, 43],
                            responseTextRange:[45, 72]
                          },
                    false, false).

test(stage_change_scope_before, [nondet]) :-
    Inp = "Before awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "before awake upon (wet) the frog shall always satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "before", ExpOut,
                    ranges{ scopeTextRange:[0, 11],
                            conditionTextRange:[13, 22],
                            componentTextRange:[24, 31],
                            timingTextRange:[39, 44],
                            responseTextRange:[46, 73]
                          },
                    false, false).

test(stage_change_scope_before_cond, [nondet]) :-
    Inp = "Before awake upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "before awake upon (wet) the frog shall always satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scs_scope(FretMent, "before", ExpOut,
                    ranges{ scopeTextRange:[0, 11],
                            conditionTextRange:[13, 22],
                            componentTextRange:[24, 31],
                            timingTextRange:[39, 44],
                            responseTextRange:[46, 73]
                          },
                    false, false).

% TODO scope only ...
% TODO scope except ...

check_scs_scope(FretMent, ScopeType, ExpOut, Ranges, Exclusive, Required) :-
    check_scs_scope(FretMent, ScopeType, ExpOut, Ranges),
    FretMent = fretment(scope_info(SFret, _SVars),
                        condition_info(_CFret, _CVars),
                        component_info(_Comp),
                        timing_info(_Timing, _TVars),
                        response_info(_Responses, _RespVars)),
    get_dict(exclusive, SFret, Excl),
    assertion(Excl == Exclusive),
    get_dict(required, SFret, Rqrd),
    assertion(Rqrd == Required).

check_scs_scope(F, S, ExpOut, Ranges) :- check_scs_scope(F, S, ["awake"], ExpOut, Ranges).

check_scs_scope(FretMent, ScopeType, ScopeVars, ExpOut, Ranges) :-
    FretMent = fretment(scope_info(SFret, SVars),
                        condition_info(CFret, CVars),
                        component_info(Comp),
                        timing_info(Timing, TVars),
                        response_info(Responses, RespVars)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == ScopeType),
    get_dict(scope_mode, SFret, SMode),
    intercalate(ScopeVars, " = ", SMVal),  % assumes either 1 or 2 vars
    assertion(SMode == SMVal),
    assertion(SVars == ScopeVars),
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
    assertion(RespVars == [ "noise", "croaking" ]),
    emit_fretish(FretMent, Out),
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, OutRanges),
    assertion(Out == Out2),
    assertion(OutRanges == Ranges).

test(stage_change_scope_invert, [nondet]) :-
    Inp = "When not in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall never satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "never", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 50],
                                   responseTextRange:[52, 79]
                                 }).

test(stage_change_scope_invert_if, [nondet]) :-
    Inp = "If not in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall never satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "never", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 50],
                                   responseTextRange:[52, 79]
                                 }).

test(stage_change_scope_invert_unless, [nondet]) :-
    Inp = "unless in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall never satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "never", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 50],
                                   responseTextRange:[52, 79]
                                 }).

check_scsi_with_timing(FretMent, WantedTiming, ExpOut, Ranges) :-
    check_scsi_with_timing_common(FretMent, Timing, [], ExpOut, Ranges),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == WantedTiming).

check_scsi_with_timing(FretMent, WantedTiming, Duration, ExpOut, Ranges) :-
    check_scsi_with_timing_common(FretMent, Timing, [], ExpOut, Ranges),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == WantedTiming),
    get_dict(duration, Timing, Dur),
    assertion(Dur == Duration).

check_scsi_with_timing(FretMent, WantedTiming, StopCond, TimingVars, ExpOut, Ranges) :-
    check_scsi_with_timing_common(FretMent, Timing, TimingVars, ExpOut, Ranges),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == WantedTiming),
    get_dict(stop_condition, Timing, SC),
    assertion(SC == StopCond).

check_scsi_with_timing_common(FretMent, Timing, TimingVars, ExpOut, Ranges) :-
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
    assertion(RespVars == [ "noise", "croaking" ]),
    emit_fretish(FretMent, Out),
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, OutRanges),
    assertion(Out == Out2),
    assertion(OutRanges == Ranges).

test(stage_change_scope_invert_eventually, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall eventually satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall eventually satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "eventually", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 55],
                                   responseTextRange:[57, 84]
                                 }).

test(stage_change_scope_invert_finally, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall finally satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall finally satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "finally", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 52],
                                   responseTextRange:[54, 81]
                                 }).

test(stage_change_scope_invert_at_the_last_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the last timepoint satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall finally satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "finally", % n.b. at the last timepoint = finally
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 52],
                                   responseTextRange:[54, 81]
                                 }).

test(stage_change_scope_invert_at_the_same_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the same timepoint satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall immediately satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately", % n.b. at the last timepoint = finally
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 56],
                                   responseTextRange:[58, 85]
                                 }).

test(stage_change_scope_invert_at_the_first_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the first timepoint satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall immediately satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately", % n.b. at the first timepoint = immediately
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 56],
                                   responseTextRange:[58, 85]
                                 }).

test(stage_change_scope_invert_initially, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall initially satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall immediately satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately", % n.b. initially = immediately
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 56],
                                   responseTextRange:[58, 85]
                                 }).

test(stage_change_scope_invert_immediately, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall immediately satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall immediately satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 56],
                                   responseTextRange:[58, 85]
                                 }).

test(stage_change_scope_invert_at_the_next_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the next timepoint satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall at the next timepoint satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "next",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 66],
                                   responseTextRange:[68, 95]
                                 }).

test(stage_change_scope_invert_within_3s, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall within 3 seconds satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall within 3  ticks satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "within", "3 ",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 60],
                                   responseTextRange:[62, 89]
                                 }).

test(stage_change_scope_invert_for_3s, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall for 3 seconds satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall for 3  ticks satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "for", "3 ",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 57],
                                   responseTextRange:[59, 86]
                                 }).

test(stage_change_scope_invert_after_3s, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall after 3 seconds satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall after 3  ticks satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "after", "3 ",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 59],
                                   responseTextRange:[61, 88]
                                 }).
    % KWQ: unrealizable?

test(stage_change_scope_invert_until, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall until !wet satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall until (! wet) satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "until", "(! wet)", ["wet"],
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 58],
                                   responseTextRange:[60, 87]
                                 }).

test(stage_change_scope_invert_before, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall before !wet satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever (wet) the frog shall before (! wet) satisfy ((noise = croaking)).",
    parse_fret("test", Inp, FretMent),
    check_scsi_with_timing(FretMent, "before", "(! wet)", ["wet"],
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 29],
                                   componentTextRange:[31, 38],
                                   timingTextRange:[46, 59],
                                   responseTextRange:[61, 88]
                                 }).

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
    assertion(RespVars == [ "noise", "croaking" ]),
    emit_fretish(FretMent, Out),
    ExpOut = "whenever (awake & persisted(2, wet)) the frog shall at the next timepoint satisfy ((noise = croaking)).",
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, Ranges),
    assertion(Out == Out2),
    assertion(Ranges == ranges{ conditionTextRange:[0, 35],
                                componentTextRange:[37, 44],
                                timingTextRange:[52, 72],
                                responseTextRange:[74, 101]
                              }).

%% Then do system pre/post

%%                    CI
%%                    RACK
%% Actual specs!
