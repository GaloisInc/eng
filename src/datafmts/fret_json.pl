% Converts parsed fretish into the on-disk JSON stored format.

:- module(fret_json, [ lando_reqs_to_fret_json/3,
                       fretment_to_fretsemantics/3,
                       replace_template_vars/5
                     ]).

:- use_module(library(apply)).
:- use_module('../englib').
:- use_module('../exprlang').
:- use_module(ltl).
:- use_module(lustre).
:- use_module(frettish).

lando_reqs_to_fret_json(Lando_Reqs, Vars, JSONDict) :-
    maplist(lando_req_to_fret_json, Lando_Reqs, JSONReqs),
    something_with(Vars, JSONVars),
    JSONDict = _{requirements: JSONReqs, variables: JSONVars}.

something_with(Vars, Vars).
lando_req_to_fret_json(Lando_Req, JSONDict) :-
    get_dict(lando_req, Lando_Req, R),
    get_dict(req_id, R, RI),
    get_dict(req_parent_id, R, PI),
    get_dict(req_name, R, RN),
    get_dict(req_desc, R, RD),
    get_dict(req_project, R, RP),
    get_dict(fret_req, R, RF),
    emit_fretish(RF, FT, Ranges),
    fretment_to_fretsemantics(RF, Ranges, FJ),
    JSONDict = _{ '_id': RI, reqid: RN, parent_reqid: PI, project: RP,
                  fulltext: FT,
                  rationale: RD, status: "", comments: "", semantics: FJ }.


% Given a fretment(scope_info(..), condition_info(..), component_info(..),
%                  timing_info(..), response_info(..))
% returns:
%   dictionary of "semantics" portion of FRET JSON for that fretment.
%
fretment_to_fretsemantics(Fretment, Ranges, JSONDict) :-
    Fretment = fretment(scope_info(Scope, _),
                        condition_info(Condition),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Response)),

    % Now "hydrate" the information with all of the additional things that the
    % FRET tool expects to find in the JSON form.

    fretment_vars(scope, FretMent, ScopeVars),
    fretment_vars(condition, FretMent, CondVars),
    fretment_vars(timing, FretMent, TimingVars),
    fretment_vars(response, FretMent, RespVars),
    append([ScopeVars, CondVars, TimingVars, RespVars], AllVars),
    list_to_set(AllVars, Vars),
    SemanticsBase = semantics{ type: "nasa",
                               % The set of variables referenced by this fret
                               % requirement; n.b. referenced by name and not
                               % by the _id.
                               variables: Vars
                             },
    (get_dict(scopeTextRange, Ranges, STR)
    -> (put_dict(Scope, _{scopeTextRange: STR}, ScopeJSON),
        put_dict(SemanticsBase, ScopeJSON, Sem1))
    ; put_dict(SemanticsBase, Scope, Sem1)
    ),

    get_dict(conditionTextRange, Ranges, CndTR),
    put_dict(Condition, _{conditionTextRange: CndTR}, ConditionJSON),
    put_dict(Sem1, ConditionJSON, Sem2),  % TODO: if no condition, this is null and breaks the next

    get_dict(componentTextRange, Ranges, CTR),
    get_dict(component, Comp, CN),
    put_dict(Comp, _{componentTextRange: CTR, component_name: CN}, CompJSON),
    put_dict(Sem2, CompJSON, Sem3),

    get_dict(timingTextRange, Ranges, TTR),
    put_dict(Timing, _{timingTextRange: TTR}, TimingJSON),
    put_dict(Sem3, TimingJSON, Sem4),

    get_dict(responseTextRange, Ranges, RTR),
    put_dict(Response, _{responseTextRange: RTR}, ResponseJSON),
    put_dict(Sem4, ResponseJSON, Sem5),

    req_semantics_defs(Sem5, SemDefs),
    create_variable_description(Sem5, DV),

    get_dict(description, SemDefs, Desc0),
    replace_template_vars(Sem5, "<b><i>", "</i></b>", Desc0, Desc),

    (generate_formulae(SemDefs, Sem5, Sem6)
    -> put_dict(_{ diagramVariables: DV,
                   description: Desc
                 }, Sem6, JSONDict)
    ; print_message(error, no_formula()), fail
    ).

prolog:message(no_formula()) --> [ 'Could not generate formulae for frettish' ].

req_semantics_defs(Inp, Semantics) :-
    get_dict(scope, Inp, S),
    get_dict(type, S, ST),
    get_dict(condition, Inp, C),
    get_dict(timing, Inp, T),
    get_dict(response, Inp, Rsp),
    atom_string(STA, ST),
    atom_string(CA, C),
    atom_string(TA, T),
    atom_string(RA, Rsp),
    lando_fret:fret_semantics(STA, CA, TA, RA, Semantics).
    %% format(atom(DefKeyA), '~w,~w,~w,~w', [ST, C, T, Rsp]),
    %% get_dict(DefKeyA, Defs, Semantics).

% ------------------------------

create_variable_description(Inp, VarDesc):-
    get_dict(scope, Inp, Scope),
    get_dict(type, Scope, ST),
    cvd_st(Inp, ST, VarDesc).

cvd_st(Inp, "null", VarDesc) :-
    !,
    get_dict(condition, Inp, C),
    cvd_cnd(Inp, C, VarDesc).
cvd_st(Inp, _, VarDesc) :-
    get_dict(condition, Inp, C),
    cvd_cnd(Inp, C, Y),
    (get_dict(scope_mode, Inp, M),
     format(atom(X), 'M = <b><i>~w</i></b>, ~w', [M, Y]),
     atom_string(X, VarDesc)
    ; VarDesc = Y
    ).

cvd_cnd(Inp, "regular", VarDesc) :-
    !,
    get_dict(timing, Inp, T),
    cvd_tim(Inp, T, Y),
    get_dict(regular_condition, Inp, RC),
    format(atom(X), 'TC = <b><i>~w</i></b>, ~w', [RC, Y]),
    atom_string(X, VarDesc).
cvd_cnd(Inp, "noTrigger", VarDesc) :-
    !,
    get_dict(timing, Inp, T),
    cvd_tim(Inp, T, Y),
    get_dict(regular_condition, Inp, RC),
    format(atom(X), 'CC = <b><i>~w</i></b>, ~w', [RC, Y]),
    atom_string(X, VarDesc).
cvd_cnd(Inp, _, VarDesc) :-
    get_dict(timing, Inp, T),
    cvd_tim(Inp, T, VarDesc).

cvd_tim(Inp, T, VarDesc) :-
    member(T, ["after", "within", "for"]),
    !,
    get_dict(response, Inp, R),
    cvd_rsp(Inp, R, Y),
    get_dict(duration, Inp, D),
    format(atom(X), 'n = <b><i>~w</i></b>, ~w', [D, Y]),
    atom_string(X, VarDesc).
cvd_tim(Inp, T, VarDesc) :-
    member(T, ["until", "before"]),
    !,
    get_dict(response, Inp, R),
    cvd_rsp(Inp, R, Y),
    get_dict(stop_condition, Inp, D),
    format(atom(X), 'SC = <b><i>~w</i></b>, ~w', [D, Y]),
    atom_string(X, VarDesc).
cvd_tim(Inp, _, VarDesc) :-
    get_dict(response, Inp, R),
    cvd_rsp(Inp, R, VarDesc).

cvd_rsp(Inp, "satisfaction", VarDesc) :-
    !,
    get_dict(post_condition, Inp, D),
    format(atom(X), 'Response = <b><i>~w</i></b>', [D]),
    atom_string(X, VarDesc).
cvd_rsp(Inp, "action", VarDesc) :-
    !,
    get_dict(action, Inp, D),
    format(atom(X), 'Response = <b><i>~w</i></b>', [D]),
    atom_string(X, VarDesc).
cvs_rsp(_, _, "").

% --------------------

% --------------------------------------------------

replace_template_vars(Inp, Pre, Post, Text, OutText) :-
    string_chars(Text, Chars),
    phrase(replVars(Inp, Pre, Post, OutChars), Chars, []),
    string_chars(OutText, OutChars).

replVars(Inp, Pre, Post, R) -->
    ['$'], w(W), ['$'],
    { !,
      atom_string(WA, W),
      get_dict(WA, Inp, V),
      format(atom(A), '~w~w~w', [Pre, V, Post]),
      atom_string(A,S)
    },
    replVars(Inp, Pre, Post, T),
    { string_chars(TS, T),
      string_concat(S, TS, R)
    }.
replVars(Inp, Pre, Post, R) -->
    [C],
    replVars(Inp, Pre, Post, T),
    { string_chars(S, [C]), string_concat(S, T, R) }.
replVars(_, _, _, "") --> [].

w(W) --> [C], { wchar(C) }, w_(CS), { string_chars(W, [C|CS]) }.
w_([C|CS]) --> [C], { wchar(C) }, !, w_(CS).
w_([]) --> [].

wchar(C) :- \+ char_type(C, space),
            % Exclude things that might be individual tokens needing to be
            % recognized elsewhere in the grammar.
            \+ member(C, ['(', ')', '.', '!', '?', ':', ',',
                          %% '{', '}', '^', '[', ']', %% XXX?
                          '$',
                          '/']).

numeric([N|NS]) --> digit(N), numeric(NS).
numeric(N) --> digit(N).
digit(D) --> [ (C) ], { char_type(C, digit),
                        atom_codes(C, [CV]),
                        atom_codes('0', [ZV]),
                        plus(ZV, D, CV)
                      }.


lxm(R) --> ws_, { ! }, lxm(R).
lxm(R) --> call(R).

lxm(R, P) --> ws_, { ! }, lxm(R, P).
lxm(R, P) --> call(R, P).

lxm(R, O, P) --> ws_, { ! }, lxm(R, O, P).
lxm(R, O, P) --> call(R, O, P).

lxm(R, O, U, P) --> ws_, { ! }, lxm(R, O, U, P).
lxm(R, O, U, P) --> call(R, O, U, P).

ws_() --> [C], { char_type(C, space) }.

% ----------------------------------------------------------------------


generate_formulae(Defs, Inp, Out) :-
    (scope_mode_transform(Defs, Inp, SMPT, SMFT, O1)
    ; print_message(error, scope_transform_failed()), fail
    ),
    !,
    (regular_condition_transform(Defs, SMPT, SMFT, O1, O2)
    ; print_message(error, condition_transform_failed(regular)), fail
    ),
    !,
    (post_condition_transform(Defs, SMPT, SMFT, O2, O3)
    ; print_message(error, condition_transform_failed(post)), fail
    ),
    !,
    (stop_condition_transform(Defs, SMPT, SMFT, O3, O4)
    ; print_message(error, condition_transform_failed(stop)), fail
    ),
    !,
    (fetched_ft_pt_update(Defs, O4, O5)
    -> Out = O5
    ; print_message(error, past_time_update_error()), fail
    ).

prolog:message(scope_transform_failed()) --> [ 'Cannot transform scope' ].
prolog:message(condition_transform_failed(Condtype)) -->
    [ 'Cannot transform ~w condition' - [ Condtype ] ].
prolog:message(past_time_update_error()) -->
    [ 'Cannot update past-time formula' ].

scope_mode_transform(_Defs, Inp, SMPT, SMFT, Out) :-
    get_dict(scope, Inp, S),
    get_dict(type, S, ST),
    sc_mo_tr(ST, Inp, SMPT, SMFT, Out).
sc_mo_tr(null, I, term(ident("BAD_PT"), bool), term(ident("BAD_FT"), bool), O) :-
    !,
    put_dict(I, _{ scope_mode_pt: "BAD_PT", scope_mode_ft: "BAD_FT" }, O).
sc_mo_tr(_, I, MP, MF, O) :-
    get_dict(scope_mode, I, MD),
    %% canon_bool_expr(M, MD),
    xform_past_temporal_unbounded(MD, MP),
    xform_future_temporal_unbounded(MD, MF),
    put_dict(I, _{ scope_mode_pt: MP, scope_mode_ft: MF }, O).

regular_condition_transform(Defs, Inp, Out) :-
    get_dict(regular_condition, Inp, RC), !,
    xform_cond(Defs, Inp, RC, PT, FT, SMVPT, SMVFT),
    put_dict(Inp, _{ regular_condition_unexp_pt: PT,
                     regular_condition_unexp_ft: FT,
                     regular_condition_SMV_pt: SMVPT,
                     regular_condition_SMV_ft: SMVFT
                   }, Out).
regular_condition_transform(_, _, _, Inp, Inp).

post_condition_transform(Defs, SMPT, SMFT, Inp, Out) :-
    get_dict(post_condition, Inp, PC), !,
    xform_cond(Defs, Inp, SMPT, SMFT, PC, PT, FT, SMVPT, SMVFT),
    emit_ltl(PT, PTTxt),
    emit_ltl(FT, FTTxt),
    emit_ltl(SMVPT, SMVPTTxt),
    emit_ltl(SMVFT, SMVFTTxt),
    put_dict(Inp, _{ post_condition_unexp_pt: PTTxt,
                     post_condition_unexp_ft: SMVPTTxt,
                     post_condition_SMV_pt: FTTxt,
                     post_condition_SMV_ft: SMVFTTxt
                   }, Out).
post_condition_transform(_, _, _, Inp, Inp).

stop_condition_transform(Defs, SMPT, SMFT, Inp, Out) :-
    get_dict(stop_condition, Inp, SC), !,
    xform_cond(Defs, Inp, SMPT, SMFT, SC, PT, FT, SMVPT, SMVFT),
    emit_ltl(PT, PTTxt),
    emit_ltl(FT, FTTxt),
    emit_ltl(SMVPT, SMVPTTxt),
    emit_ltl(SMVFT, SMVFTTxt),
    put_dict(Inp, _{ stop_condition_unexp_pt: PTTxt,
                     stop_condition_unexp_ft: SMVPTTxt,
                     stop_condition_SMV_pt: FTTxt,
                     stop_condition_SMV_ft: SMVFTTxt
                   }, Out).
stop_condition_transform(_, _, _, Inp, Inp).

xform_cond(Defs, Inp, SMPT, SMFT, C, PT, FT, SMVPT, SMVFT) :-
    xform_past_temporal(C, CP),
    xform_future_temporal(C, CF),

    get_dict(endpoints, Defs, Endpoints),
    get_dict(left, Endpoints, Left),
    get_dict(right, Endpoints, Right),
    get_dict('SMVptExtleft', Endpoints, SMVLeft),
    %% get_dict('SMVptExtright', Endpoints, SMVRight),
    %% get_dict('SMVftExtleft2', Endpoints, SMVLeftF),
    get_dict('SMVftExtright2', Endpoints, SMVRightF),
    % All the above are strings from the semantics.json file.  Convert them to
    % LTL ABT's, but don't backtrack beyond this point on parse failures (not
    % expecting any!).
    !,
    parse_ltl(Left, LeftLTL),
    parse_ltl(SMVLeft, SMVLeftLTL),
    parse_ltl(Right, RightLTL),
    parse_ltl(SMVRightF, SMVRightLTL),
    !,

    ltl_langdef(LTLLang),
    get_dict(language, LTLLang, LTL),

    subst_term(LTL, "$Left$", LeftLTL, CP, CP1),
    subst_term(LTL, "$scope_mode$", SMPT, CP1, PT),
    subst_term(LTL, "$Left$", SMVLeftLTL, CP, CPS1),
    subst_term(LTL, "$scope_mode$", SMPT, CPS1, SMVPT),

    subst_term(LTL, "$Right$", RightLTL, CF, CF1),
    subst_term(LTL, "$scope_mode$", SMFT, CF1, FT),
    subst_term(LTL, "$Right$", SMVRightLTL, CF, CFS1),
    subst_term(LTL, "$scope_mode$", SMFT, CFS1, SMVFT).

fetched_ft_pt_update(Defs, Inp, Out) :-
    get_dict(ft, Defs, FT),
    subst("$regular_condition$", "$regular_condition_unexp_ft$", FT, F2),
    subst("$post_condition$", "$post_condition_unexp_ft$", F2, F3),
    subst("$stop_condition$", "$stop_condition_unexp_ft$", F3, F4),
    subst("$scope_mode$", "$scope_mode_ft$", F4, FTF),
    replace_template_vars(Inp, "", "", FTF, TFT),

    get_dict(pt, Defs, PT),
    subst("$regular_condition$", "$regular_condition_unexp_pt$", PT, P2),
    subst("$post_condition$", "$post_condition_unexp_pt$", P2, P3),
    subst("$stop_condition$", "$stop_condition_unexp_pt$", P3, P4),
    subst("$scope_mode$", "$scope_mode_pt$", P4, PTF),
    replace_template_vars(Inp, "", "", PTF, TPT),

    get_dict(ptExpanded, Defs, PTE),
    subst("$regular_condition$", "$regular_condition_SMV_pt$", PTE, PE2),
    subst("$post_condition$", "$post_condition_SMV_pt$", PE2, PE3),
    subst("$stop_condition$", "$stop_condition_SMV_pt$", PE3, PE4),
    subst("$scope_mode$", "$scope_mode_pt$", PE4, PTEF),
    replace_template_vars(Inp, "", "", PTEF, TPET),
    salt_to_smv(TPET, PSMV),
    transform_to_AST(PSMV, PASTRaw),
    xform_past_optimize(PASTRaw, PAST),
    ast_to_LTL(PAST, PLTL),
    ltl_ast_to_CoCo(PAST, PCOCO),

    get_dict(ftExpanded, Defs, FTE),
    subst("$regular_condition$", "$regular_condition_SMV_ft$", FTE, FE2),
    subst("$post_condition$", "$post_condition_SMV_ft$", FE2, FE3),
    subst("$stop_condition$", "$stop_condition_SMV_ft$", FE3, FE4),
    subst("$scope_mode$", "$scope_mode_ft$", FE4, FTEF),
    replace_template_vars(Inp, "", "", FTEF, TFET),
    salt_to_smv(TFET, FSMV),
    xform_future_optimize(FSMV, FOPT),

    get_dict(ftInfAUExpanded, Defs, FAU),
    subst("$regular_condition$", "$regular_condition_SMV_ft$", FAU, FAU2),
    subst("$post_condition$", "$post_condition_SMV_ft$", FAU2, FAU3),
    subst("$stop_condition$", "$stop_condition_SMV_ft$", FAU3, FAU4),
    subst("$scope_mode$", "$scope_mode_ft$", FAU4, FAUF),
    replace_template_vars(Inp, "", "", FAUF, TFAU),
    last_is_FALSE(TFAU, TFAULF),
    salt_to_smv(TFAULF, FAUSMV),
    xform_future_optimize(FAUSMV, FAUOPT),

    % XXX: if constants.generateBetweenSemantics is set, more should be added
    % here, see SemanticsAnalyzer.js

    put_dict(Inp, _{ ft_fetched: FTF,
                     ft: TFT,
                     pt_fetched: PTF,
                     pt: TPT,
                     ptExpanded_fetched: PTEF,
                     ptExpanded: PLTL,
                     'CoCoSpecCode': PCOCO,
                     ftExpanded_fetched: FTEF,
                     ftExpandedUnoptimized: FSMV,
                     ftExpanded: FOPT,
                     ftInfAUExpanded_fetched: FAUF,
                     ftInfAUExpanded: FAUOPT
                   }, Out).

canon_bool_expr(E, O) :-
    special_char_subst(E, E0),
    subst(' true ', ' TRUE ', E0, E1),
    subst(' false ', ' FALSE ', E1, O).

special_char_subst(I, I).  % TODO: translate special chars, see fret-electron/support/utils.js

last_is_FALSE(I, O) :- subst(' LAST ', 'FALSE', I, O).

% --------------------

xform_past_temporal_unbounded(I, I).  % TODO (only for scope mode)
xform_future_temporal_unbounded(I, I). % TODO

xform_past_temporal(I, O) :-  % provided/returns string
    parse_ltl(I, AST),
    fmap(fret_json:xpt, AST, XAST),
    !,
    emit_ltl(XAST, O).
xpt(boolcall("persisted", [val(V),A2]),
    and(ltlH_bound(salt_le(val(V)), A2),
        ltlH_bound(salt_lt(val(V)), not(boolid("$Left$"))))).
xpt(boolcall("occurred", [val(V),A2]),
    and(ltlS(not(boolid("$Left$")), A2),
        ltlO_bound(salt_le(val(V)), A2))).
xpt(boolcall("preBool", [Init, P]),
    or(and(not(ltlY(true)), Init),
       and(not(not(ltlY(true))), ltlY(P)))).
xpt(boolcall("prevOcc", [P, Q]),
    or(boolid("$Left$"),
       ltlY(implies(ltlS(and(not(boolid("$Left$")), not(P)), P),
                    ltlS(and(not(boolid("$Left$")), not(P)), and(P, Q)))))).
xpt(E, E).

xform_future_temporal(I, I). % TODO

xform_past_optimize(I, I). % provided/returns AST; already done by ltl_parse

xform_future_optimize(I, I).  % TODO xform.transform(X, xform.optimizeFT)

conjunction([], "").
conjunction([E|ES], C) :-
    conjunction(ES, ESC),
    (ESC == ""
    -> format(atom(CA), "~w", [E])
    ; format(atom(CA), "(~w) & (~w)", [E, ESC])
    ),
    atom_string(CA, C).

% --------------------

salt_to_smv(I, I).
salt_to_smv_DISABLED(I, SMV) :-   % XXX: test this
    string_chars(I, CS),
    phrase(salt2smv(O), CS, R),
    !,
    ( R == []
    -> string_chars(SMV, O)
    ; format('  SMV: ~w~n  REMAINDER: ~w~n', [ SMV, R ]),
      string_chars(SMV, O)
    ).

salt2smv(SMV) --> [ '[', '<', '=' ], lxm(numeric, N), lxm(endSquarePlusOne), % KWQ: actually, arithEx...
                  salt2smv(Rest),
                  { succ(N, M),
                    format(atom(X), '[0,~w]~w~n', [M, Rest]),
                    atom_chars(X, SMV)
                  }.
salt2smv(SMV) --> [ '[', '<', '=' ], lxm(numeric, N), lxm(endSquare),
                  salt2smv(Rest),
                  { format(atom(X), '[0,~w]~w~n', [N, Rest]),
                    atom_chars(X, SMV)
                  }.
salt2smv(SMV) --> [ '[', '<' ], lxm(numeric, N), lxm(endSquarePlusOne),
                  salt2smv(Rest),
                  { format(atom(X), '[0,~w]~w~n', [N, Rest]),
                    atom_chars(X, SMV)
                  }.
salt2smv(SMV) --> [ '[', '<' ], lxm(numeric, N), lxm(endSquare),
                  salt2smv(Rest),
                  succ(P, N),
                  { format(atom(X), '[0,~w]~w~n', [P, Rest]),
                    atom_chars(X, SMV)
                  }.
salt2smv(SMV) --> [ '[', '=' ], lxm(numeric, N), lxm(endSquarePlusOne),
                  salt2smv(Rest),
                  { succ(N, M),
                    format(atom(X), '[~w,~w]~w~n', [M, M, Rest]),
                    atom_chars(X, SMV)
                  }.
salt2smv(SMV) --> [ '[', '=' ], numeric(N), lxm(endSquare),
                  salt2smv(Rest),
                  format(atom(X), '[~w,~w]~w~n', [N, N, Rest]), atom_chars(X, SMV).
salt2smv([S|MV]) --> [ S ], salt2smv(MV).
salt2smv([]) --> [].

endSquarePlusOne() --> [ '+', '1', ']' ].
endSquare() --> [ ']' ].


% --------------------

transform_to_AST(I, AST) :-
    subst('=>', '->', I, I1),
    (parse_ltl(I1, AST), !
    ; print_message(error, cannot_parse_ltl(I1)), fail
    ).

prolog:message(cannot_parse_ltl(I)) --> [ 'Unable to parse LTL: ~w' - [I] ].

ast_to_LTL(I, O) :- emit_ltl(I, O).

ltl_ast_to_CoCo(I, O) :- emit_CoCoSpec(I, O).
