:- module(lando_fret, [ lando_to_fret/2,
                        parse_fret/3
                      ]).

:- use_module(library(http/json)).
:- use_module('datafmts/lando').
:- use_module('englib').

lando_to_fret(LandoSSL, fret{ requirements:FretRequirements,
                              variables:FretVariables }) :-
    get_dict(body, LandoSSL, Body),
    get_semantics_defs(SemanticDefs),
    phrase(extract_fret("Default", "Default",
                        SemanticDefs, FretRequirements, FretVars),
           Body, Remaining),
    ( Remaining == []
    -> cross_reference(FretRequirements, FretVars, FretVariables)
    ; format('Unexpected extra not converted to Fret: ~w~n', [Remaining])
    ).

resource(fret_semantics, 'src/semantics.json').

cross_reference(_, [], []).
cross_reference(Reqs, [V|Vars], [O|OutVars]) :-
    cross_reference(Reqs, Vars, OutVars),
    get_dict(variable_name, V, VarName),
    var_ref_req_ids(VarName, Reqs, RefIds),
    put_dict(V, _{ reqs: RefIds }, O).

var_ref_req_ids(_, [], []).
var_ref_req_ids(VarName, [R|Reqs], [RefID|RefIds]) :-
    get_dict(variables, R, VNames),
    member(VarName, VNames), !,
    get_dict('_id', R, RefID),
    var_ref_req_ids(VarName, Reqs, RefIds).
var_ref_req_ids(VarName, [_|Reqs], RefIds) :-
    var_ref_req_ids(VarName, Reqs, RefIds).

% ----------------------------------------------------------------------

get_semantics_defs(Defs) :-
    open('res://lando_fret:fret_semantics', read, Strm),
    json_read_dict(Strm, Defs).

extract_fret(ProjName, FretCompName, Defs, Reqs, Vars) -->
    [ SpecElement ],
    { is_dict(SpecElement, SpecType),
      member(SpecType, [ system, subsystem ]), %%%% <- selector
      !,
      specElement_ref(SpecElement, SysName),
      get_dict(body, SpecElement, SysBody),
      (ProjName == "Default" -> NewProjName = SysName ; NewProjName = ProjName),
      phrase(extract_fret(NewProjName, SysName, Defs, SysReqs, SysVars), SysBody, _Remaining)
    },
    extract_fret(ProjName, FretCompName, Defs, NextReqs, NextVars),
    { append(SysReqs, NextReqs, Reqs),
      append(SysVars, NextVars, Vars)  % KWQ TODO normalize vars
    }.
extract_fret(ProjName, FretCompName, Defs, Reqs, Vars) -->
    [ SpecElement ],
    { is_dict(SpecElement, requirement), %%%% <- selector
      !,
      specElement_ref(SpecElement, Name),
      get_dict(explanation, SpecElement, Expl),
      get_dict(uid, SpecElement, UID),
      get_dict(indexing, SpecElement, Indexing),
      make_fret_reqs(Defs, ProjName, FretCompName, Name, Expl, UID, Indexing, ThisReqs, ThisVars)
    },
    extract_fret(ProjName, FretCompName, Defs, NextReqs, NextVars),
    { append(ThisReqs, NextReqs, Reqs),
      append(ThisVars, NextVars, Vars)  % KWQ TODO normalize vars
    }.
extract_fret(ProjName, FretCompName, Defs, Reqs, Vars) -->
    [ SpecElement ],
    { is_dict(SpecElement, component), %%%% <- selector
      fret_usage_type(SpecElement, Usage, Type, ModeReqs),
      !,
      specElement_ref(SpecElement, VarName),
      get_dict(explanation, SpecElement, Expl),
      format(atom(IDA), "~w~w~w", [ ProjName, FretCompName, VarName ]),
      atom_string(IDA, ID),
      Var = _{ project: ProjName,
               component_name: FretCompName,
               variable_name: VarName,
               dataType: Type,
               idType: Usage,
               description: Expl,
               '_id': ID,
               modeRequirement: ModeReqs,
               assignment: "", % XXX
               copilotAssignment: "",
               modeldoc: false,
               modeldoc_id: "",
               modelComponent: "",
               completed: true
             }
    },
    extract_fret(ProjName, FretCompName, Defs, Reqs, NextVars),
    { append([Var], NextVars, Vars)  % KWQ TODO normalize vars
    }.
extract_fret(_, _, _, [], []) --> [ _ ].
extract_fret(_, _, _, [], []) --> [].


fret_usage_type(Element, Usage, Type, ModeReqs) :-
    get_dict(features, Element, Features),
    fret_var_usage_feature(Features, Usage, ModeReqs),
    fret_var_type_feature(Features, Type).

fret_var_usage_feature(Features, Usage, ModeReqs) :-
    member(Feature, Features),
    fret_var_usage_(Features, Feature, Usage, ModeReqs).
fret_var_usage_(Features, Feature, Usage, ModeReqs) :-
    get_dict(text, Feature, T),
    string_concat(Before, " var.", T),
    string_concat("FRET ", Usage, Before),
    var_modereqs(Features, Usage, ModeReqs).

var_modereqs(Features, "Mode", ModeReqs) :-
    !,
    findall(T, regular_constraints(Features, T), TS),
    conjunction(TS, ModeReqs).
var_modereqs(_, _, "").

regular_constraints(Features, Constraint) :-
    member(Feature, Features),
    get_dict(text, Feature, T),
    \+ string_concat("FRET ", _, T),
    string_concat(Constraint, ".", T).

fret_var_type_feature(Features, Type) :-
    member(Feature, Features),
    fret_var_type_(Feature, Type).
fret_var_type_(Feature, Type) :-
    get_dict(text, Feature, T),
    string_concat("FRET : ", FT, T),
    string_concat(Type, ".", FT).

make_fret_reqs(_Defs, _ProjName, _CompName, _ReqName, _Expl, _UID, [], [], []).
make_fret_reqs(Defs, ProjName, _CompName, ReqName, Expl, UID, [Index|_IXS], [Req], Vars) :-
    get_dict(key, Index, "FRET"),
    !,
    get_dict(values, Index, Lines),
    intercalate(Lines, " ", English),
    parse_fret(Defs, English, FretReq),
    format(atom(ReqID), '~w-req-~w', [ ProjName, UID ]),
    %% KWQ: TODO: process _IXS for more FRET reqs (they need to be numbered as sub-reqs of the main requirement
    Req = requirement{ reqid: ReqName,
                       % parent_reqid:?
                       project: ProjName,
                       rationale: Expl,
                       fulltext: English,
                       status: "",
                       comments: "",
                       '_id': ReqID,
                       semantics: FretReq
                     },
    Vars = [].
make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID, [_|IXS], Reqs, Vars) :-
    make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID, IXS, Reqs, Vars).


% ----------------------------------------------------------------------

%% Parses a Frettish English requirement to a Fret structured requirement, using
%% the definitions and templates provided to enhance the structured requirement.
parse_fret(Defs, English, FretRequirement) :-  % KWQ: return vars as well
    string_chars(English, ECodes),
    enumerate(ECodes, Input),
    phrase(frettish(Defs, Semantics), Input, Remaining),
    ( Remaining == [], ! ;
      format('Unexpected frettish extra not parsed: ~w~n', [ Remaining ])
    ),
    FretRequirement = Semantics.

enumerate(I, O) :- enum_(0, I, O).
enum_(_, [], []).
enum_(N, [I|IS], [(N,I)|OS]) :- succ(N, M), enum_(M, IS, OS).

% scope conditions component shall timing responses
frettish(Defs, Semantics) -->
    scope(Scope, ScopeVars),
    %% {format('....scope: ~w~n', [Scope])},
    conditions(Condition, CondVars),
    %% {format('....conditions: ~w with ~w~n', [Condition, CondVars])},
    component(Comp),
    %% {format('....component: ~w~n', [Comp])},
    lexeme(shall, _),
    !,
    timing(Timing),
    %% {format('....timing: ~w~n', [Timing])},
    responses(Responses, RespVars),
    %% {format('....responses: ~w with ~w~n', [Responses, RespVars])},
    make_fret_req(Defs, Scope, ScopeVars, Condition, CondVars, Comp, Timing,
                  Responses, RespVars, Semantics).
% scope conditions shall component timing responses
frettish(Defs, Semantics) -->
    scope(Scope, ScopeVars),
    %% {format('....scope: ~w~n', [Scope])},
    conditions(Condition, CondVars),
    %% {format('....conditions: ~w with ~w~n', [Condition, CondVars])},
    lexeme(shall, _),
    !,
    component(Comp),
    %% {format('....component: ~w~n', [Comp])},
    timing(Timing),
    %% {format('....timing: ~w~n', [Timing])},
    responses(Responses, RespVars),
    %% {format('....responses: ~w with ~w~n', [Responses, RespVars])},
    make_fret_req(Defs, Scope, ScopeVars, Condition, CondVars, Comp, Timing,
                  Responses, RespVars, Semantics).

make_fret_req(Defs, Scope, ScopeVars, Condition, CondVars, Comp, Timing,
              Response, RespVars, Semantics) -->
    { append([ScopeVars, CondVars, RespVars], AllVars),
      list_to_set(AllVars, Vars),
      SemanticsBase = semantics{ type: "nasa",
                                 variables: Vars
                               },
      put_dict(SemanticsBase, Scope, Sem1),
      put_dict(Sem1, Condition, Sem2),
      put_dict(Sem2, Comp, Sem3),
      put_dict(Sem3, Response, Sem4),
      put_dict(Sem4, Timing, Sem5),

      req_semantics_defs(Defs, Sem5, SemDefs),
      create_variable_description(Sem5, DV),

      get_dict(description, SemDefs, Desc0),
      replace_template_vars(Sem5, "<b><i>", "</i></b>", Desc0, Desc),

      generate_formulae(SemDefs, Sem5, Sem6),

      put_dict(_{ diagramVariables: DV,
                  description: Desc
                }, Sem6, Semantics)
    }.

% --------------------

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
    get_dict(scope_mode, Inp, M),
    format(atom(X), 'M = <b><i>~w</i></b>, ~w', [M, Y]),
    atom_string(X, VarDesc).

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

req_semantics_defs(Defs, Inp, Semantics) :-
    get_dict(scope, Inp, S),
    get_dict(type, S, ST),
    get_dict(condition, Inp, C),
    get_dict(timing, Inp, T),
    get_dict(response, Inp, Rsp),
    format(atom(DefKeyA), '~w,~w,~w,~w', [ST, C, T, Rsp]),
    get_dict(DefKeyA, Defs, Semantics).

% --------------------

generate_formulae(Defs, Inp, Out) :-
    scope_mode_transform(Defs, Inp, O1),
    regular_condition_transform(Defs, O1, O2),
    post_condition_transform(Defs, O2, O3),
    stop_condition_transform(Defs, O3, O4),
    fetched_ft_pt_update(Defs, O4, O5),
    Out = O5.

scope_mode_transform(_Defs, Inp, Out) :-
    get_dict(scope, Inp, S),
    get_dict(type, S, ST),
    sc_mo_tr(ST, Inp, Out).
sc_mo_tr("null", I, O) :-
    !, put_dict(I, _{ scope_mode_pt: "BAD_PT", scope_mode_ft: "BAD_FT" }, O).
sc_mo_tr(_, I, O) :-
    get_dict(scope_mode, I, M),
    canon_bool_expr(M, MD),
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
regular_condition_transform(_Defs, Inp, Inp).

post_condition_transform(Defs, Inp, Out) :-
    get_dict(post_condition, Inp, PC), !,
    xform_cond(Defs, Inp, PC, PT, FT, SMVPT, SMVFT),
    put_dict(Inp, _{ post_condition_unexp_pt: PT,
                     post_condition_unexp_ft: SMVPT,
                     post_condition_SMV_pt: FT,
                     post_condition_SMV_ft: SMVFT
                   }, Out).
post_condition_transform(_Defs, Inp, Inp).

stop_condition_transform(Defs, Inp, Out) :-
    get_dict(stop_condition, Inp, SC), !,
    xform_cond(Defs, Inp, SC, PT, FT, SMVPT, SMVFT),
    put_dict(Inp, _{ stop_condition_unexp_pt: PT,
                     stop_condition_unexp_ft: SMVPT,
                     stop_condition_SMV_pt: FT,
                     stop_condition_SMV_ft: SMVFT
                   }, Out).
stop_condition_transform(_Defs, Inp, Inp).

xform_cond(Defs, Inp, C, PT, FT, SMVPT, SMVFT) :-
    xform_past_temporal(C, CP),
    xform_future_temporal(C, CF),

    get_dict(endpoints, Defs, Endpoints),
    get_dict(left, Endpoints, Left),
    get_dict(right, Endpoints, Right),
    get_dict('SMVptExtleft', Endpoints, SMVLeft),
    %% get_dict('SMVptExtright', Endpoints, SMVRight),
    %% get_dict('SMVftExtleft2', Endpoints, SMVLeftF),
    get_dict('SMVftExtright2', Endpoints, SMVRightF),
    get_dict(scope_mode_pt, Inp, ScopeModePT),
    get_dict(scope_mode_ft, Inp, ScopeModeFT),

    subst("$Left$", Left, CP, CP1),
    subst("$scope_mode$", ScopeModePT, CP1, PT),
    subst("$Left$", SMVLeft, CP, CPS1),
    subst("$scope_mode$", ScopeModePT, CPS1, SMVPT),

    subst("$Right$", Right, CF, CF1),
    subst("$scope_mode$", ScopeModeFT, CF1, FT),
    subst("$Right$", SMVRightF, CF, CFS1),
    subst("$scope_mode$", ScopeModeFT, CFS1, SMVFT).

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
    transform_to_AST(PSMV, PAST),
    ast_to_LTL(PAST, PLTL),
    ast_to_CoCo(PAST, PCOCO),

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

salt_to_smv(I, I).  % TODO
transform_to_AST(I, I).  % TODO
ast_to_LTL(I, I).  % TODO
ast_to_CoCo(I, I). % TODO
xform_past_temporal_unbounded(I, I).  % TODO
xform_future_temporal_unbounded(I, I). % TODO
xform_past_temporal(I, I).  % TODO
xform_future_temporal(I, I). % TODO
xform_past_optimize(I, I).  % TODO xform.transform(X, xform.optimizePT)
xform_future_optimize(I, I).  % TODO xform.transform(X, xform.optimizeFT)

% --------------------------------------------------

scope(Scope, Vars) -->
    scope_(SubScope, Vars, Pos), opt_comma(_),
    { range(Pos, Range),
      put_dict(SubScope, _{scopeTextRange:Range}, Scope)
    }.
scope(_{type: null}, []) --> [].

scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars, Pos) -->
    in(P0), scope_mode(Mode, Vars, PM), { pos(P0, PM, Pos) }.
scope_(_{scope:{type: "in"}, scope_mode:Mode}, Vars, Pos) -->
    during(P0), scope_mode(Mode, Vars, PM), { pos(P0, PM, Pos) }.
% scope_(TODO) --> TODO.

scope_mode(Mode, [Mode], P) --> lexeme(mode, P0), lexeme(word, Mode, PE),
                                { pos(P0, PE, P) }.
scope_mode(Mode, [Mode], P) --> lexeme(word, Mode, P0), lexeme(mode, PE),
                                { pos(P0, PE, P) }.
scope_mode(Mode, [Mode], P) --> lexeme(word, Mode, P).

% --------------------------------------------------

conditions(ReqCond, Vars) -->
    and(P0), cond_(C,CP,AllVars),
    { pos(P0, CP, P),
      range(P, Range),
      list_to_set(AllVars, Vars),
      put_dict(C, _{condition:"regular", conditionTextRange:Range}, ReqCond )
    }.
conditions(ReqCond, Vars) -->
    cond_(C,CP,AllVars),
    { range(CP, Range),
      list_to_set(AllVars, Vars),
      put_dict(C, _{condition:"regular", conditionTextRange:Range}, ReqCond )
    }.
conditions(null, []) --> [].

cond_(C,CP,V) -->
    qcond1_(C0,CP0,V0), opt_comma(_), qcond2_(C0,CP0,V0,C,CP,V), opt_comma(_).
cond_(C,CP,V) --> qcond1_(C,CP,V), opt_comma(_).

qcond1_(C,P,Vars) -->
    lexeme(qualifier, Q, QP),
    lexeme(precond, E, Vars, _), lexeme(is, _), lexeme(true, EP),
    { !,
      pos(QP, EP, P),
      qcond1_true_(Q,E,C)
    }.
qcond1_(C,P,Vars) -->
    lexeme(qualifier, Q, QP),
    lexeme(precond, E, Vars, _), lexeme(is, _), lexeme(false, EP),
    { !,
      pos(QP, EP, P),
      format(atom(PCA), "(!(~w))", [E]), atom_string(PCA, PC), % n.b. negated
      C = _{ qualifier_word:Q,
             pre_condition: PC,
             regular_condition: PC
           }
    }.
qcond1_(C,P,Vars) -->
    lexeme(qualifier, Q, QP), lexeme(precond, E, Vars, CP),
    { pos(QP, CP, P),
      qcond1_true_(Q,E,C)
    }.
qcond1_true_(Q,E,C) :-
    format(atom(PCA), "(~w)", [E]), atom_string(PCA, PC),
    C = _{ qualifier_word:Q,
           pre_condition: PC,
           regular_condition: PC
         }.

qcond2_(C0,P0,V0,C,P,Vars) --> and(_), qcond2_and_(C0,P0,V0,C,P,Vars).
qcond2_(C0,P0,V0,C,P,Vars) -->
    or(_),
    qcond1_(C1,P1,V1),
    { pos(P0, P1, P),
      get_dict(pre_condition, C0, C0P),
      get_dict(pre_condition, C1, C1P),
      format(atom(PCA), '((~w) | (~w))', [ C0P, C1P ]), atom_string(PCA, PC),
      get_dict(qualifier_word, C1, C1QW),
      append(V0, V1, Vars),
      C = _{ qualifier_word: C1QW,  % XXX: always just uses *last* qualifier?!
             pre_condition: PC,
             regular_condition: PC
           }
    }.
qcond2_(C0,P0,V0,C,P,Vars) --> qcond2_and_(C0,P0,V0,C,P,Vars).
qcond2_and_(C0,P0,V0,C,P,Vars) -->
    qcond1_(C1,P1,V1),
    { pos(P0, P1, P),
      get_dict(pre_condition, C0, C0P),
      get_dict(pre_condition, C1, C1P),
      format(atom(PCA), '((~w) & (~w))', [ C0P, C1P ]), atom_string(PCA, PC),
      get_dict(qualifier_word, C1, C1QW),
      append(V0, V1, Vars),
      C = _{ qualifier_word: C1QW,  % XXX: always just uses *last* qualifier?!
             pre_condition: PC,
             regular_condition: PC
           }
    }.

qualifier("upon", P) --> upon(P).
qualifier("whenever", P) --> whenever(P).
qualifier("when", P) --> when(P).
qualifier("unless", P) --> unless(P).
qualifier("where", P) --> where(P).
qualifier("if", P) --> if(P).

precond(E, V, P) --> bool_expr(E, V, P).

% --------------------------------------------------

component(_{ component: Comp,
             component_name: Comp,
             componentTextRange:Range }) -->
    the(TP), lexeme(word, Comp, CP), { pos(TP, CP, P), range(P, Range) }.
component(_{ component: Comp,
             component_name: Comp,
             componentTextRange:Range }) -->
    word(Comp, P), { range(P, Range) }.

% --------------------------------------------------

timing(_{ timing: "next", timingTextRange:Range}) -->
    lexeme(at, SP), lexeme(the, _), lexeme(next, _), lexeme(timepoint, TP),
    { pos(SP, TP, P), range(P, Range) }.
timing(_{ timing: "always"}) --> [].


% --------------------------------------------------

responses(_{response: "satisfaction",
            post_condition: E,
            responseTextRange: Range
           }, Vars) -->
    lexeme(satisfy, SP), !, postcond(EP, AllVars, CP),
    { format(atom(EA), '(~w)', [EP]), atom_string(EA, E),
      pos(SP, CP, P),
      list_to_set(AllVars, Vars),
      range(P, Range)
    }.

postcond(E, V, P) --> bool_expr(E, V, P).

% --------------------------------------------------

%% bool_expr(E, V, P) --> [ (N,'!') ], bool_expr(E, PE, V), { pos(N, PE, P) }.
%% bool_expr(E, Vars, P) --> [ (N,'~') ], bool_expr(E, PE, V), { pos(N, PE, P) }.
bool_expr(V, [V], P) --> lexeme(word, V, P).

conjunction([], "").
conjunction([E|ES], C) :-
    conjunction(ES, ESC),
    (ESC == ""
    -> format(atom(CA), "~w", [E])
    ; format(atom(CA), "(~w) & (~w)", [E, ESC])
    ),
    atom_string(CA, C).

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

w(W) --> [C], { word_char(C) }, w_(CS), { string_chars(W, [C|CS]) }.
w_([C|CS]) --> [C], { word_char(C) }, !, w_(CS).
w_([]) --> [].

% --------------------------------------------------

range(span(S,E), [S,E]).

opt_comma(P) --> [(N,',')], ws(PE), { pos(N, PE, P) }.
opt_comma(P) --> ws(P).

and(P) --> token("and", P).
at(P) --> token("at", P).
during(P) --> token("during", P).
false(P) --> token("false", P).
if(P) --> token("if", P).
in(P) --> token("in", P).
is(P) --> token("is", P).
mode(P) --> token("mode", P).
next(P) --> token("next", P).
or(P) --> token("or", P).
satisfy(P) --> token("satisfy", P).
shall(P) --> token("shall", P).
the(P) --> token("the", P).
timepoint(P) --> token("timepoint", P).
true(P) --> token("true", P).
unless(P) --> token("unless", P).
upon(P) --> token("upon", P).
whenever(P) --> token("whenever", P).
when(P) --> token("when", P).
where(P) --> token("where", P).

token(M,P) --> word(W,P), { any_case_match([M], W) }.

any_case_match(Candidates, Word) :- to_lower(Word, LCWord),
                                    member(LCWord, Candidates).

to_lower(I, O) :- atom_string(IA, I), downcase_atom(IA, OA), atom_string(OA, O).

word(W,P) --> [(N,C)], { word_char(C) }, wc(CS,PE),
              { string_codes(W, [C|CS]), pos(N, PE, P) }.
wc([C|CS],P) --> [(N,C)], { word_char(C) }, !, wc(CS,LP), { pos(N, LP, P) }.
wc([],span(0,0)) --> [].

word_char(C) :- \+ char_type(C, space),
                % Exclude things that might be individual tokens needing to be
                % recognized elsewhere in the grammar.
                \+ member(C, ['(', ')', '.', '!', '?', ':', ',',
                              %% '{', '}', '^', '[', ']', %% XXX?
                              '$',
                              '/']).

lexeme(R, P) --> ws(_), { ! }, lexeme(R, P).
lexeme(R, P) --> call(R, P).

lexeme(R, O, P) --> ws(_), { ! }, lexeme(R, O, P).
lexeme(R, O, P) --> call(R, O, P).

lexeme(R, O, U, P) --> ws(_), { ! }, lexeme(R, O, U, P).
lexeme(R, O, U, P) --> call(R, O, U, P).

%% wsp(P) --> ws(A), ws(B), { pos(A,B,P) }.
%% wsp(P) --> ws(P).

ws(span(N,N)) --> [(N,C)], { char_type(C, space) }.

pos(span(S,E), span(0,0), span(S,E)) :- !.
pos(span(S,_), span(_,E), span(S,E)) :- !.
pos(S, span(0,0), span(S,S)) :- !.
pos(S, span(_,E), span(S,E)).
