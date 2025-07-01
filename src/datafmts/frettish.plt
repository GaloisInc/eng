:- begin_tests(frettish).
:- use_module(frettish).
:- use_module('../englib').
:- use_module('../exprlang').
:- use_module(library(strings)).

% These are defined by exprlang, but their precedence is lost on import:
:- op(760, yfx, ⦂).
:- op(750, xfy, →).

test(state_change, [nondet]) :-
    Inp = "frog shall always satisfy if (wet & awake) then (noise = croaking)",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    FretMent = fretment(scope_info(SFret, _),
                        condition_info(CFret),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Responses)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == null),
    fretment_vars(scope, FretMent, SVars),
    assertion(SVars == []),
    get_dict(condition, CFret, Condition),
    assertion(Condition == "null"),
    fretment_vars(condition, FretMent, CVars),
    assertion(CVars == []),
    get_dict(component, Comp, CompName),
    assertion(CompName == "frog"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "always"),
    fretment_vars(timing, FretMent, TVars),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond ==
              fretish(op(implies( op(and(term(ident("wet"), bool),
                                         term(ident("awake"), bool)),
                                     bool),
                                  op(eq(term(ident("noise"), type_unassigned('⚲T2')),
                                        term(ident("croaking"), type_unassigned('⚲T2'))),
                                     bool)),
                         bool))),
    fretment_vars(response, FretMent, RespVars),
    assertion(RespVars == [ "wet" ⦂ bool,
                            "awake" ⦂ bool,
                            "noise" ⦂ type_unassigned('⚲T2'),
                            "croaking" ⦂ type_unassigned('⚲T2') ]),
    emit_fretish(FretMent, Out),
    ExpOut = "the frog shall always satisfy (wet & awake) => (noise = croaking).",
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, Ranges),
    assertion(Out == Out2),
    assertion(Ranges == ranges{ conditionTextRange:[0, 0],
                                componentTextRange:[0, 7],
                                timingTextRange:[15, 20],
                                responseTextRange:[22, 64]
                              }).

test(state_change_noparen_expr, [nondet]) :-
    Inp = "frog shall always satisfy if wet & awake then noise = croaking",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    FretMent = fretment(scope_info(SFret, _),
                        condition_info(CFret),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Responses)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == null),
    fretment_vars(scope, FretMent, SVars),
    assertion(SVars == []),
    get_dict(condition, CFret, Condition),
    assertion(Condition == "null"),
    fretment_vars(condition, FretMent, CVars),
    assertion(CVars == []),
    get_dict(component, Comp, CompName),
    assertion(CompName == "frog"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "always"),
    fretment_vars(timing, FretMent, TVars),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond ==
              fretish(op(implies( op(and(term(ident("wet"), bool),
                                         term(ident("awake"), bool)),
                                     bool),
                                  op(eq(term(ident("noise"), type_unassigned('⚲T2')),
                                        term(ident("croaking"), type_unassigned('⚲T2'))),
                                     bool)),
                         bool))),
    fretment_vars(response, FretMent, RespVars),
    assertion(RespVars == [ "wet" ⦂ bool,
                            "awake" ⦂ bool,
                            "noise" ⦂ type_unassigned('⚲T2'),
                            "croaking" ⦂ type_unassigned('⚲T2') ]),
    emit_fretish(FretMent, Out),
    ExpOut = "the frog shall always satisfy (wet & awake) => (noise = croaking).",
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, Ranges),
    assertion(Out == Out2),
    assertion(Ranges == ranges{ conditionTextRange:[0, 0],
                                componentTextRange:[0, 7],
                                timingTextRange:[15, 20],
                                responseTextRange:[22, 64]
                              }).

test(stage_change_natural, [nondet]) :-
    Inp = "Upon wet & awake the frog shall always satisfy (noise = croaking)",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    FretMent = fretment(scope_info(SFret, _),
                        condition_info(CFret),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Responses)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == null),
    fretment_vars(scope, FretMent, SVars),
    assertion(SVars == []),
    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "regular"),
    assertion(Qualifier == "upon"),
    assertion(RegCond == fretish(op(and(term(ident("wet"), bool),
                                        term(ident("awake"), bool)),
                                    bool))),
    assertion(RegCond == PreCond),
    fretment_vars(condition, FretMent, CVars),
    assertion(CVars == ["wet" ⦂ bool, "awake" ⦂ bool]),
    get_dict(component, Comp, CompName),
    assertion(CompName == "frog"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "always"),
    fretment_vars(timing, FretMent, TVars),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond ==
              fretish(op(eq(term(ident("noise"), type_unassigned('⚲T2')),
                            term(ident("croaking"), type_unassigned('⚲T2'))),
                         bool))),
    fretment_vars(response, FretMent, RespVars),
    assertion(RespVars == [ "noise" ⦂ type_unassigned('⚲T2'),
                            "croaking" ⦂ type_unassigned('⚲T2') ]),
    emit_fretish(FretMent, Out),
    ExpOut = "upon (wet & awake) the frog shall always satisfy (noise = croaking).",
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, Ranges),
    assertion(Out == Out2),
    assertion(Ranges == ranges{ conditionTextRange:[0, 17],
                                componentTextRange:[19, 26],
                                timingTextRange:[34, 39],
                                responseTextRange:[41, 66]
                              }).

%% ---------------------------------------- SCOPE TESTS

test(stage_change_scope, [nondet]) :-
    Inp = "In awake upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "in awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "in", ExpOut, ranges{ scopeTextRange:[0, 7],
                                                    conditionTextRange:[9, 16],
                                                    componentTextRange:[18, 25],
                                                    timingTextRange:[33, 38],
                                                    responseTextRange:[40, 65]
                                                  }).

test(expr_scope, [nondet]) :-
    Inp = "while mind = awake upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "while (mind = awake) upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "in", [ "mind" ⦂ type_unassigned('⚲T0'),
                                      "awake" ⦂ type_unassigned('⚲T0')
                                    ], ExpOut,
                    ranges{ scopeTextRange:[0, 19],
                            conditionTextRange:[21, 28],
                            componentTextRange:[30, 37],
                            timingTextRange:[45, 50],
                            responseTextRange:[52, 77]
                          }).

test(expr_inv_scope, [nondet]) :-
    Inp = "except while mind = awake upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "except while (mind = awake) upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "notin", [ "mind" ⦂ type_unassigned('⚲T0'),
                                         "awake" ⦂ type_unassigned('⚲T0')
                                       ], ExpOut,
                    ranges{ scopeTextRange:[0, 26],
                            conditionTextRange:[28, 35],
                            componentTextRange:[37, 44],
                            timingTextRange:[52, 57],
                            responseTextRange:[59, 84]
                          }).

test(stage_change_scope_during, [nondet]) :-
    Inp = "During awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "in awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "in", ExpOut,
                    ranges{ scopeTextRange:[0, 7],
                            conditionTextRange:[9, 16],
                            componentTextRange:[18, 25],
                            timingTextRange:[33, 38],
                            responseTextRange:[40, 65]
                          }).

test(stage_change_scope_when_in, [nondet]) :-
    Inp = "when in awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "in awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "in", ExpOut,
                    ranges{ scopeTextRange:[0, 7],
                            conditionTextRange:[9, 16],
                            componentTextRange:[18, 25],
                            timingTextRange:[33, 38],
                            responseTextRange:[40, 65]
                          }).

test(stage_change_scope_if_in, [nondet]) :-
    Inp = "if in awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "in awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "in", ExpOut,
                    ranges{ scopeTextRange:[0, 7],
                            conditionTextRange:[9, 16],
                            componentTextRange:[18, 25],
                            timingTextRange:[33, 38],
                            responseTextRange:[40, 65]
                          }).

test(stage_change_scope_only_in, [nondet]) :-
    Inp = "only in awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "only in awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "onlyIn", ExpOut,
                    ranges{ scopeTextRange:[0, 12],
                            conditionTextRange:[14, 21],
                            componentTextRange:[23, 30],
                            timingTextRange:[38, 43],
                            responseTextRange:[45, 70]
                          }).

test(stage_change_scope_only_if_in, [nondet]) :-
    Inp = "only if in awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "only in awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "onlyIn", ExpOut,
                    ranges{ scopeTextRange:[0, 12],
                            conditionTextRange:[14, 21],
                            componentTextRange:[23, 30],
                            timingTextRange:[38, 43],
                            responseTextRange:[45, 70]
                          }).

test(stage_change_scope_only_when_in, [nondet]) :-
    Inp = "only when in awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "only in awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "onlyIn", ExpOut,
                    ranges{ scopeTextRange:[0, 12],
                            conditionTextRange:[14, 21],
                            componentTextRange:[23, 30],
                            timingTextRange:[38, 43],
                            responseTextRange:[45, 70]
                          }).

test(stage_change_scope_only_during, [nondet]) :-
    Inp = "only during awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "only in awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "onlyIn", ExpOut,
                    ranges{ scopeTextRange:[0, 12],
                            conditionTextRange:[14, 21],
                            componentTextRange:[23, 30],
                            timingTextRange:[38, 43],
                            responseTextRange:[45, 70]
                          }).

test(stage_change_scope_in, [nondet]) :-
    Inp = "Unless in awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "unless in awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "notin", ExpOut,
                    ranges{ scopeTextRange:[0, 14],
                            conditionTextRange:[16, 23],
                            componentTextRange:[25, 32],
                            timingTextRange:[40, 45],
                            responseTextRange:[47, 72]
                          }).

test(stage_change_scope_after, [nondet]) :-
    Inp = "After awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "after awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "after", ExpOut,
                    ranges{ scopeTextRange:[0, 10],
                            conditionTextRange:[12, 19],
                            componentTextRange:[21, 28],
                            timingTextRange:[36, 41],
                            responseTextRange:[43, 68]
                          },
                    false, false).

test(stage_change_scope_only_after, [nondet]) :-
    Inp = "Only after awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "only after awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "onlyAfter", ExpOut,
                    ranges{ scopeTextRange:[0, 15],
                            conditionTextRange:[17, 24],
                            componentTextRange:[26, 33],
                            timingTextRange:[41, 46],
                            responseTextRange:[48, 73]
                          },
                    false, false).

test(stage_change_scope_before, [nondet]) :-
    Inp = "Before awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "before awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "before", ExpOut,
                    ranges{ scopeTextRange:[0, 11],
                            conditionTextRange:[13, 20],
                            componentTextRange:[22, 29],
                            timingTextRange:[37, 42],
                            responseTextRange:[44, 69]
                          },
                    false, false).

test(stage_change_scope_only_before, [nondet]) :-
    Inp = "Only before awake mode upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "only before awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "onlyBefore", ExpOut,
                    ranges{ scopeTextRange:[0, 16],
                            conditionTextRange:[18, 25],
                            componentTextRange:[27, 34],
                            timingTextRange:[42, 47],
                            responseTextRange:[49, 74]
                          },
                    false, false).

test(stage_change_scope_before_cond, [nondet]) :-
    % n.b. no "mode" word
    Inp = "Before awake upon wet the frog shall always satisfy (noise = croaking)",
    ExpOut = "before awake upon wet the frog shall always satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scs_scope(FretMent, "before",
                    ExpOut,
                    ranges{ scopeTextRange:[0, 11],
                            conditionTextRange:[13, 20],
                            componentTextRange:[22, 29],
                            timingTextRange:[37, 42],
                            responseTextRange:[44, 69]
                          },
                    false, false).

check_scs_scope(FretMent, ScopeType, ExpOut, Ranges, Exclusive, Required) :-
    check_scs_scope(FretMent, ScopeType, ExpOut, Ranges),
    FretMent = fretment(scope_info(SFret, _SVars),
                        condition_info(_CFret),
                        component_info(_Comp),
                        timing_info(_Timing, _TVars),
                        response_info(_Responses)),
    get_dict(scope, SFret, Scp),
    get_dict(exclusive, Scp, Excl),
    assertion(Excl == Exclusive),
    get_dict(required, Scp, Rqrd),
    assertion(Rqrd == Required).

check_scs_scope(F, S, ExpOut, Ranges) :-
    check_scs_scope(F, S, ["awake" ⦂ bool], ExpOut, Ranges).

check_scs_scope(FretMent, ScopeType, ScopeVars, ExpOut, Ranges) :-
    FretMent = fretment(scope_info(SFret, _),
                        condition_info(CFret),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Responses)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == ScopeType),
    get_dict(scope_mode, SFret, SMode),
    length(ScopeVars, SVLen),  % tests use either 1 or 2 vars
    (SVLen == 1
    -> (ScopeVars = [SV1⦂SVTy],
        (ScopeType == "before"
        -> assertion(member(SMode,
                            % with "mode" in FRETish, this gets parsed as a
                            % mode(_), but without "mode" the boolexpr
                            % alternative parses this as an term(ident(_)).  Both
                            % need to be recognized as valid.
                            [mode(SV1),
                             fretish(term(ident(SV1), SVTy))
                            ])) % boolexpr on mode before mode parse
        ; assertion(SMode == mode(SV1))
        )
       )
    ; ScopeVars = [SV1⦂SVTy, SV2⦂SVTy],
      assertion(SMode == fretish(op(eq(term(ident(SV1), SVTy),
                                       term(ident(SV2), SVTy)),
                                    bool)))
    ),
    fretment_vars(scope, FretMent, SVars),
    assertion(SVars == ScopeVars),
    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "regular"),
    assertion(Qualifier == "upon"),
    assertion(PreCond == fretish(term(ident("wet"), bool))),
    assertion(RegCond == PreCond),
    fretment_vars(condition, FretMent, CVars),
    assertion(CVars == ["wet" ⦂ bool]),
    get_dict(component, Comp, CompName),
    assertion(CompName == "frog"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "always"),
    fretment_vars(timing, FretMent, TVars),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, fretish(PostCond)),
    assertion(Rspns == "satisfaction"),
    TNum is 1 + ((SVLen - 1) * 2),
    % TNum is if scope is a mode; if it is an expression, then we might need TNum
    % + 1 (because expressions tested here will have a single variable).
    OrTNum is TNum + 1,
    format(atom(UTID), '⚲T~w', [TNum]),
    format(atom(UTID2), '⚲T~w', [OrTNum]),
    assertion(member(PostCond,
                     [ op(eq(term(ident("noise"), type_unassigned(UTID)),
                             term(ident("croaking"), type_unassigned(UTID))),
                          bool),
                       op(eq(term(ident("noise"), type_unassigned(UTID2)),
                             term(ident("croaking"), type_unassigned(UTID2))),
                          bool)
                     ])),
    fretment_vars(response, FretMent, RespVars),
    assertion(member(RespVars, [ [ "noise" ⦂ type_unassigned(UTID),
                                   "croaking" ⦂ type_unassigned(UTID) ],
                                 [ "noise" ⦂ type_unassigned(UTID2),
                                   "croaking" ⦂ type_unassigned(UTID2) ]
                               ])),
    emit_fretish(FretMent, Out),
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, OutRanges),
    assertion(Out == Out2),
    assertion(OutRanges == Ranges).

test(stage_change_scope_invert, [nondet]) :-
    Inp = "When not in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall never satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "never", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 48],
                                   responseTextRange:[50, 75]
                                 }).

test(stage_change_scope_invert_if, [nondet]) :-
    Inp = "If not in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall never satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "never", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 48],
                                   responseTextRange:[50, 75]
                                 }).

test(stage_change_scope_invert_unless, [nondet]) :-
    Inp = "unless in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall never satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "never", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 48],
                                   responseTextRange:[50, 75]
                                 }).

test(stage_change_scope_except_in, [nondet]) :-
    Inp = "Except in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall never satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "never", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 48],
                                   responseTextRange:[50, 75]
                                 }).

test(stage_change_scope_except_if_in, [nondet]) :-
    Inp = "Except if in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall never satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "never", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 48],
                                   responseTextRange:[50, 75]
                                 }).

test(stage_change_scope_except_when_in, [nondet]) :-
    Inp = "Except when in awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall never satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "never", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 48],
                                   responseTextRange:[50, 75]
                                 }).

test(stage_change_scope_except_during, [nondet]) :-
    Inp = "Except during awake mode whenever wet the frog shall never satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall never satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "never", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 48],
                                   responseTextRange:[50, 75]
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
    FretMent = fretment(scope_info(SFret, _),
                        condition_info(CFret),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Responses)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == "notin"),
    get_dict(scope_mode, SFret, SMode),
    assertion(SMode == mode("awake")),
    fretment_vars(scope, FretMent, SVars),
    assertion(SVars == ["awake" ⦂ bool]),

    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "holding"),
    assertion(Qualifier == "whenever"),
    assertion(PreCond == fretish(term(ident("wet"), bool))),
    assertion(RegCond == PreCond),
    fretment_vars(condition, FretMent, CVars),
    assertion(CVars == ["wet"⦂bool]),

    get_dict(component, Comp, CompName),
    assertion(CompName == "frog"),

    fretment_vars(timing, FretMent, TVars),
    assertion(TVars == TimingVars),

    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond ==
              fretish(op(eq(term(ident("noise"), type_unassigned('⚲T1')),
                            term(ident("croaking"), type_unassigned('⚲T1'))),
                         bool))),
    fretment_vars(response, FretMent, RespVars),
    assertion(RespVars == [ "noise" ⦂ type_unassigned('⚲T1'),
                            "croaking" ⦂ type_unassigned('⚲T1')
                          ]),
    emit_fretish(FretMent, Out),
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, OutRanges),
    assertion(Out == Out2),
    assertion(OutRanges == Ranges).

%% ---------------------------------------- EXPRESSION TESTS

test(bool_exprs, [nondet]) :-
    Inp = "Before (awake | light) | !true upon wet | persisted(3, damp) the frog shall always satisfy ((noise = croaking) & (!asleep) | (noise != silent) & (noise > silent) & (noise = (307 - 5 + 6 / 1 * 2 ^ 32345)) & !false)",
    ExpOut = "before ((awake | light) | (! true)) upon (wet | persisted(3, damp)) the frog shall always satisfy ((noise = croaking) & ((! asleep) | ((noise != silent) & ((noise > silent) & ((noise = (307 - (5 + (6 / (1 * (2 ^ 32345)))))) & (! false)))))).",
    Ranges = ranges{ scopeTextRange:[0, 34],
                     conditionTextRange:[36, 66],
                     componentTextRange:[68, 75],
                     timingTextRange:[83, 88],
                     responseTextRange:[90, 239]
                   },
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    ScopeVars = ["awake"⦂bool, "light"⦂bool ],
    FretMent = fretment(scope_info(SFret, _),
                        condition_info(CFret),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Responses)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == "before"),
    get_dict(scope_mode, SFret, SMode),
    assertion(SMode == fretish(op(or(op(or(term(ident("awake"), bool),
                                           term(ident("light"), bool)),
                                        bool),
                                     op(not(term(lit(true), bool)), bool)),
                                  bool))),
    fretment_vars(scope, FretMent, SVars),
    assertion(SVars == ScopeVars),
    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "regular"),
    assertion(Qualifier == "upon"),
    assertion(RegCond ==
              fretish(op(or(term(ident("wet"), bool),
                            op(persisted(term(num(3), integer),
                                         term(ident("damp"), bool)),
                               bool)),
                         bool))),
    assertion(RegCond == PreCond),
    fretment_vars(condition, FretMent, CVars),
    assertion(CVars == ["wet"⦂bool, "damp"⦂bool]),
    get_dict(component, Comp, CompName),
    assertion(CompName == "frog"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "always"),
    fretment_vars(timing, FretMent, TVars),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond ==
              fretish(op(and(op(eq(term(ident("noise"), integer),
                                   term(ident("croaking"), integer)), bool),
                             op(or(op(not(term(ident("asleep"), bool)),
                                      bool),
                                   op(and(op(neq(term(ident("noise"), integer),
                                                 term(ident("silent"), integer)),
                                             bool),
                                          op(and(op(gt(term(ident("noise"), integer),
                                                       term(ident("silent"), integer)),
                                                    bool),
                                                 op(and(op(eq(term(ident("noise"), integer),
                                                              op(sub(term(num(307), integer),
                                                                     op(add(term(num(5), integer),
                                                                            op(divide(term(num(6), integer),
                                                                                      op(mul(term(num(1), integer),
                                                                                             op(expo(term(num(2), integer),
                                                                                                     term(num(32345), integer)),
                                                                                                integer)),
                                                                                         integer)),
                                                                               integer)),
                                                                        integer)),
                                                                 integer)),
                                                           bool),
                                                        op(not(term(lit(false), bool)),
                                                           bool)),
                                                    bool)),
                                             bool)),
                                      bool)),
                                bool)),
                         bool))),
    fretment_vars(response, FretMent, RespVars),
    assertion(RespVars == [ "noise"⦂integer, "croaking"⦂integer,
                            "asleep"⦂bool, "silent"⦂integer ]),
    emit_fretish(FretMent, Out),
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, OutRanges),
    assertion(Out == Out2),
    assertion(OutRanges == Ranges),
    get_dict(exclusive, Scope, Excl),
    assertion(Excl == false),
    get_dict(required, Scope, Rqrd),
    assertion(Rqrd == false).

%% ---------------------------------------- TIMING TESTS

test(stage_change_scope_invert_eventually, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall eventually satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall eventually satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "eventually", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 53],
                                   responseTextRange:[55, 80]
                                 }).

test(stage_change_scope_invert_finally, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall finally satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall finally satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "finally", ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 50],
                                   responseTextRange:[52, 77]
                                 }).

test(stage_change_scope_invert_at_the_last_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the last timepoint satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall finally satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "finally", % n.b. at the last timepoint = finally
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 50],
                                   responseTextRange:[52, 77]
                                 }).

test(stage_change_scope_invert_at_the_same_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the same timepoint satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall immediately satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately", % n.b. at the last timepoint = finally
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 54],
                                   responseTextRange:[56, 81]
                                 }).

test(stage_change_scope_invert_at_the_first_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the first timepoint satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall immediately satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately", % n.b. at the first timepoint = immediately
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 54],
                                   responseTextRange:[56, 81]
                                 }).

test(stage_change_scope_invert_initially, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall initially satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall immediately satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately", % n.b. initially = immediately
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 54],
                                   responseTextRange:[56, 81]
                                 }).

test(stage_change_scope_invert_immediately, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall immediately satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall immediately satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "immediately",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 54],
                                   responseTextRange:[56, 81]
                                 }).

test(stage_change_scope_invert_at_the_next_timepoint, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall at the next timepoint satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall at the next timepoint satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "next",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 64],
                                   responseTextRange:[66, 91]
                                 }).

test(stage_change_scope_invert_within_3s, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall within 3 seconds satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall within 3  ticks satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "within", "3 ",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 58],
                                   responseTextRange:[60, 85]
                                 }).

test(stage_change_scope_invert_within_321s, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall within 321 seconds satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall within 321  ticks satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "within", "321 ",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 60],
                                   responseTextRange:[62, 87]
                                 }).

test(stage_change_scope_invert_for_3s, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall for 3 seconds satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall for 3  ticks satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "for", "3 ",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 55],
                                   responseTextRange:[57, 82]
                                 }).

test(stage_change_scope_invert_after_3s, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall after 3 seconds satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall after 3  ticks satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "after", "3 ",
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 57],
                                   responseTextRange:[59, 84]
                                 }).
    % KWQ: unrealizable?

test(stage_change_scope_invert_until, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall until !wet satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall until (! wet) satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "until",
                           fretish(op(not(term(ident("wet"), bool)), bool)),
                           ["wet"⦂bool],
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 56],
                                   responseTextRange:[58, 83]
                                 }).

test(stage_change_scope_invert_before, [nondet]) :-
    % n.b. identical to stage_change_scope_invert except timing
    Inp = "When not in awake mode whenever wet the frog shall before !wet satisfy (noise = croaking)",
    ExpOut = "unless in awake whenever wet the frog shall before (! wet) satisfy (noise = croaking).",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    check_scsi_with_timing(FretMent, "before",
                           fretish(op(not(term(ident("wet"), bool)), bool)),
                           ["wet"⦂bool],
                           ExpOut,
                           ranges{ scopeTextRange:[0, 14],
                                   conditionTextRange:[16, 27],
                                   componentTextRange:[29, 36],
                                   timingTextRange:[44, 57],
                                   responseTextRange:[59, 84]
                                 }).

%% capture above 3 in FRET, then validate frettish can parse.  Then return to scope.  And finally condition exprs.

test(with_ltl_predicate, [nondet]) :-
    Inp = "whenever awake & persisted(2, wet) the frog shall at the next timepoint satisfy (noise = croaking)",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    FretMent = fretment(scope_info(SFret, _),
                        condition_info(CFret),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Responses)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == null),
    fretment_vars(scope, FretMent, SVars),
    assertion(SVars == []),
    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "holding"),
    assertion(Qualifier == "whenever"),
    assertion(RegCond ==
              fretish(
                  op(and(term(ident("awake"), bool),
                         op(persisted(term(num(2), integer),
                                      term(ident("wet"), bool)), bool)),
                     bool))),
    assertion(RegCond == PreCond),
    fretment_vars(condition, FretMent, CVars),
    assertion(CVars == ["awake"⦂bool, "wet"⦂bool]),
    get_dict(component, Comp, CompName),
    assertion(CompName == "frog"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "next"),
    fretment_vars(timing, FretMent, TVars),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"),
    assertion(PostCond ==
              fretish(op(eq(term(ident("noise"), type_unassigned('⚲T2')),
                            term(ident("croaking"), type_unassigned('⚲T2'))),
                         bool))),
    fretment_vars(response, FretMent, RespVars),
    assertion(RespVars == [ "noise"⦂type_unassigned('⚲T2'),
                            "croaking"⦂type_unassigned('⚲T2') ]),
    emit_fretish(FretMent, Out),
    ExpOut = "whenever (awake & persisted(2, wet)) the frog shall at the next timepoint satisfy (noise = croaking).",
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, Ranges),
    assertion(Out == Out2),
    assertion(Ranges == ranges{ conditionTextRange:[0, 35],
                                componentTextRange:[37, 44],
                                timingTextRange:[52, 72],
                                responseTextRange:[74, 99]
                              }).

test(in_upon_next_satisfy, [nondet]) :-
    Inp = "in braking upon start_button the Car shall at the next timepoint satisfy is_moving.",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env),
    parse_fret("test", Env, Inp, FretMent),
    FretMent = fretment(scope_info(SFret, _),
                        condition_info(CFret),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Responses)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == "in"), %%
    fretment_vars(scope, FretMent, SVars),
    assertion(SVars == ["braking"⦂bool]),
    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "regular"), %%
    assertion(Qualifier == "upon"),
    assertion(PreCond == RegCond),
    assertion(RegCond == fretish(term(ident("start_button"), bool))),
    fretment_vars(condition, FretMent, CVars),
    assertion(CVars == ["start_button"⦂bool]),
    get_dict(component, Comp, CompName),
    assertion(CompName == "Car"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "next"), %%
    fretment_vars(timing, FretMent, TVars),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"), %%
    assertion(PostCond == fretish(term(ident("is_moving"), bool))),
    fretment_vars(response, FretMent, RespVars),
    assertion(RespVars == [ "is_moving"⦂bool ]),
    emit_fretish(FretMent, Out),
    ExpOut = "in braking upon start_button the Car shall at the next timepoint satisfy is_moving.",
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, Ranges),
    assertion(Out == Out2),
    assertion(Ranges == ranges{ scopeTextRange:[0,9],
                                conditionTextRange:[11, 27],
                                componentTextRange:[29, 35],
                                timingTextRange:[43, 63],
                                responseTextRange:[65, 81]
                              }).

test(initial_env_type, [nondet]) :-
    Inp = "in braking upon start_button the Car shall at the next timepoint satisfy is_moving.",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env0),
    fresh_var(Env0, "braking", mode, Env1),
    fresh_var(Env1, "start_button", bool, Env2),
    fresh_var(Env2, "braking", mode, Env),  % redefine is fine
    parse_fret("test", Env, Inp, FretMent),
    FretMent = fretment(scope_info(SFret, _),
                        condition_info(CFret),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Responses)),
    get_dict(scope, SFret, Scope),
    get_dict(type, Scope, SType),
    assertion(SType == "in"), %%
    fretment_vars(scope, FretMent, SVars),
    assertion(SVars == ["braking"⦂bool]),
    get_dict(condition, CFret, Condition),
    get_dict(qualifier_word, CFret, Qualifier),
    get_dict(pre_condition, CFret, PreCond),
    get_dict(regular_condition, CFret, RegCond),
    assertion(Condition == "regular"), %%
    assertion(Qualifier == "upon"),
    assertion(PreCond == RegCond),
    assertion(RegCond == fretish(term(ident("start_button"), bool))),
    fretment_vars(condition, FretMent, CVars),
    assertion(CVars == ["start_button"⦂bool]),
    get_dict(component, Comp, CompName),
    assertion(CompName == "Car"),
    get_dict(timing, Timing, Tmng),
    assertion(Tmng == "next"), %%
    fretment_vars(timing, FretMent, TVars),
    assertion(TVars == []),
    get_dict(response, Responses, Rspns),
    get_dict(post_condition, Responses, PostCond),
    assertion(Rspns == "satisfaction"), %%
    assertion(PostCond == fretish(term(ident("is_moving"), bool))),
    fretment_vars(response, FretMent, RespVars),
    assertion(RespVars == [ "is_moving"⦂bool ]),
    emit_fretish(FretMent, Out),
    ExpOut = "in braking upon start_button the Car shall at the next timepoint satisfy is_moving.",
    assertion(Out == ExpOut),
    emit_fretish(FretMent, Out2, Ranges),
    assertion(Out == Out2),
    assertion(Ranges == ranges{ scopeTextRange:[0,9],
                                conditionTextRange:[11, 27],
                                componentTextRange:[29, 35],
                                timingTextRange:[43, 63],
                                responseTextRange:[65, 81]
                              }).

test(initial_env_bad_type, [nondet,
                            error(invalid_term_type(fretish_expr, bool,
                                                    term(ident("start_button"), integer)))
                           ]) :-
    Inp = "in braking upon start_button the Car shall at the next timepoint satisfy is_moving.",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env0),
    fresh_var(Env0, "braking", mode, Env1),
    fresh_var(Env1, "start_button", integer, Env2),
    fresh_var(Env2, "braking", mode, Env3),  % redefine is fine
    fresh_var(Env3, "skidding", out, Env),  % unused var is ignored
    parse_fret("test", Env, Inp, _).

test(initial_env_unknown_type, [nondet,
                                error(unknown_expr_type(fretish_expr,
                                                        term(ident("start_button"), int),
                                                        bool))
                               ]) :-
    Inp = "in braking upon start_button the Car shall at the next timepoint satisfy is_moving.",
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    initial_gamma(Env0),
    fresh_var(Env0, "braking", mode, Env1),
    fresh_var(Env1, "start_button", int, Env2), % unknown type
    fresh_var(Env2, "skidding", out, Env),  % unused var is ignored
    parse_fret("test", Env, Inp, _).


%% Then do system pre/post

%%                    CI
%%                    RACK
%% Actual specs!
