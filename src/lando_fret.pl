:- module(lando_fret, [ lando_to_fret/2 ]).

:- use_module(library(http/json)).
:- use_module('datafmts/frettish').
:- use_module('datafmts/lando').
:- use_module('datafmts/ltl').
:- use_module('englib').

lando_to_fret(LandoSSL, fret{ requirements:FretRequirements,
                              variables:FretVariables }) :-
    get_dict(body, LandoSSL, Body),
    get_semantics_defs(SemanticDefs),
    phrase(extract_fret("Default", "Default",
                        SemanticDefs, FretRequirements, FretVars, FretModes),
           Body, Remaining),
    fret_results(Remaining, FretVars, FretModes, FretRequirements, FretVariables).

fret_results([], FretVars, FretModes, FretRequirements, FretVariables) :-
    !,
    modes_to_vars(FretVars, FretModes, ModeVars),
    append(FretVars, ModeVars, AllVars),
    cross_reference(FretRequirements, AllVars, FretVariables).
fret_results(Remaining, _, _, _, _) :-
    format('Unexpected extra not converted to Fret: ~w~n', [Remaining]),
    fail.

resource(fret_semantics, 'src/semantics.json').

% --------------------

cross_reference(_, [], []).
cross_reference(Reqs, [V|Vars], [O|OutVars]) :-
    cross_reference(Reqs, Vars, OutVars),
    get_dict(variable_name, V, VarName),
    var_ref_req_ids(VarName, Reqs, RefIds),
    put_dict(V, _{ reqs: RefIds }, O).

var_ref_req_ids(_, [], []).
var_ref_req_ids(VarName, [noreq|Reqs], RefIds) :-
    !,
    var_ref_req_ids(VarName, Reqs, RefIds).
var_ref_req_ids(VarName, [R|Reqs], [RefID|RefIds]) :-
    get_dict(semantics, R, Semantics),
    get_dict(variables, Semantics, VNames),
    member(VarName, VNames), !,
    get_dict('_id', R, RefID),
    var_ref_req_ids(VarName, Reqs, RefIds).
var_ref_req_ids(VarName, [_|Reqs], RefIds) :-
    var_ref_req_ids(VarName, Reqs, RefIds).

% --------------------

modes_to_vars(_, [], []).
modes_to_vars(Vars, [Mode|Modes], OutVars) :-
    mode_vars(Vars, Mode, ModeVars), !,
    modes_to_vars(Vars, Modes, MVars),
    append(ModeVars, MVars, OutVars).
modes_to_vars(Vars, [Mode|Modes], OutVars) :-
    print_message(warning, no_var_for_mode(Mode)),
    modes_to_vars(Vars, Modes, OutVars).

prolog:message(no_var_for_mode(Mode)) -->
    { Mode = (ModeName, _) },
    [ 'No variable definition (component) for Mode (scenario) ~w~n' - [ ModeName ] ].

mode_vars(Vars, (VarName, VarVals), MVars) :-
    find_var(Vars, VarName, TgtVar),
    get_dict(project, TgtVar, Proj),
    get_dict(component_name, TgtVar, Comp),
    Type = "boolean", % XXX?  or is it the type of TgtVar?
    mkModeVars(Proj, Comp, VarName, VarVals, Type, MVars).

mkModeVars(_, _, _, [], _, []).
mkModeVars(Proj, Comp, VarName, [(MName, MDesc, MVal)|MVals], Type, [Var|MVars]) :-
    format(atom(X), '~w = ~w', [VarName, MVal]),
    atom_string(X, XS),
    mkVar(Proj, Comp, MName, MDesc, Type, "Mode", XS, Var),
    mkModeVars(Proj, Comp, VarName, MVals, Type, MVars).

find_var([V|_], Name, V) :- get_dict(variable_name, V, Name), !.
find_var([_|VS], Name, V) :- find_var(VS, Name, V).

    
% ----------------------------------------------------------------------

get_semantics_defs(Defs) :-
    open('res://lando_fret:fret_semantics', read, Strm),
    json_read_dict(Strm, Defs).

extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, Modes) -->
    [ SpecElement ],
    { is_dict(SpecElement, SpecType),
      member(SpecType, [ system, subsystem ]), %%%% <- selector
      !,
      specElement_ref(SpecElement, SysName),
      get_dict(body, SpecElement, SysBody),
      (ProjName == "Default" -> NewProjName = SysName ; NewProjName = ProjName),
      phrase(extract_fret(NewProjName, SysName, Defs, SysReqs, SysVars, SysModes), SysBody, _Remaining)
    },
    extract_fret(ProjName, FretCompName, Defs, NextReqs, NextVars, NextModes),
    { append(SysReqs, NextReqs, Reqs),
      append(SysVars, NextVars, Vars),  % KWQ TODO normalize vars
      append(SysModes, NextModes, Modes)
    }.
extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, Modes) -->
    [ SpecElement ],
    { is_dict(SpecElement, requirement), %%%% <- selector
      !,
      specElement_ref(SpecElement, Name),
      get_dict(explanation, SpecElement, Expl),
      get_dict(uid, SpecElement, UID),
      get_dict(indexing, SpecElement, Indexing),
      make_fret_reqs(Defs, ProjName, FretCompName, Name, Expl, UID, 0, Indexing, ThisReqs)
    },
    extract_fret(ProjName, FretCompName, Defs, NextReqs, Vars, Modes),
    { prepend_valid_reqs(ThisReqs, NextReqs, Reqs) }.
extract_fret(ProjName, FretCompName, Defs, Reqs, [Var|Vars], Modes) -->
    [ SpecElement ],
    { is_dict(SpecElement, component), %%%% <- selector
      fret_usage_type(SpecElement, Usage, Type, ModeReqs),
      !,
      specElement_ref(SpecElement, VarName),
      get_dict(explanation, SpecElement, Expl),
      mkVar(ProjName, FretCompName, VarName, Expl, Type, Usage, ModeReqs, Var)
    },
    extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, Modes).
extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, [Mode|Modes]) -->
    [ SpecElement ],
    { is_dict(SpecElement, scenarios), %%%% <- selector
      get_dict(name, SpecElement, ScenarioName),
      string_concat(Var, " Modes", ScenarioName),
      !,
      get_dict(scenarios, SpecElement, Scenarios),
      var_modes(Scenarios, 0, VarModes),
      Mode = (Var, VarModes)
    },
    extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, Modes).
extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, Modes) -->
    [ _ ],  % skip other elements
    extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, Modes).
extract_fret(_, _, _, [], [], []) --> [].

prepend_valid_reqs([], Reqs, Reqs).
prepend_valid_reqs([noreq|RS], Reqs, OutReqs) :-
    prepend_valid_reqs(RS, Reqs, OutReqs).
prepend_valid_reqs([R|RS], Reqs, [R|OutRS]) :-
    prepend_valid_reqs(RS, Reqs, OutRS).

mkVar(ProjName, FretCompName, VarName, Expl, Type, Usage, ModeReqs, Var) :-
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
             moduleName: "",
             modeldoc: false,
             modeldoc_id: "",
             modelComponent: "",
             completed: true
           }.

var_modes([], _, []).
var_modes([Scenario|Scenarios], ModeVal, [(ModeName, ModeDesc, ModeVal)|VModes]) :-
    get_dict(id, Scenario, ModeName),
    get_dict(text, Scenario, ModeDesc),
    succ(ModeVal, NextModeVal),
    var_modes(Scenarios, NextModeVal, VModes).

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

make_fret_reqs(_, _, _, _, _, _, _, [], []).
make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID, Num, [Index|IXS], [Req|Reqs]) :-
    get_dict(key, Index, "FRET"),
    !,
    parse_fret_or_error(Defs, ProjName, CompName, ReqName, Expl, UID, Num,
                        Index, Req),
    succ(Num, NextNum),
    make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID, NextNum, IXS, Reqs).
make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID, Num, [_|IXS], Reqs) :-
    make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID, Num, IXS, Reqs).


parse_fret_or_error(Defs, ProjName, CompName, ReqName, Expl, UID, Num, Index, Req) :-
    get_dict(values, Index, Lines),
    get_dict(pos, Index, pos{line:Line, col:_Col}),
    format(atom(Context), 'line ~w (~w.~w.~w #~w)',
           [ Line, ProjName, CompName, ReqName, Num ]),
    intercalate(Lines, " ", English),
    parse_fret(Context, English, FretMent),
    !,
    ( fretment_semantics(Defs, FretMent, FretReq)
    -> (Num == 0
       -> format(atom(ReqID), '~w-req-~w', [ ProjName, UID ]), RName = ReqName
       ; format(atom(ReqID), '~w-req-~w-~w', [ ProjName, UID, Num ]),
         format(atom(RName), '~w-~w', [ ReqName, Num ])
       ),
       Req = requirement{ reqid: RName,
                          % parent_reqid:?  % KWQ: TODO
                          project: ProjName,
                          rationale: Expl,
                          fulltext: English,
                          status: "",
                          comments: "",
                          '_id': ReqID,
                          semantics: FretReq
                        }
    ; print_message(error, no_semantics(Context, English, FretMent))
    ).
parse_fret_or_error(_, ProjName, CompName, ReqName, _, _, _, Index, noreq) :-
    get_dict(values, Index, Lines),
    intercalate(Lines, " ", English),
    print_message(error, bad_frettish(ProjName, CompName, ReqName, English)).

prolog:message(no_semantics(Context, English, FretMent)) -->
    [ 'Unable to determine semantics for ~w: ~w~n   :: ~w~n'
      - [ Context, English, FretMent ] ].
prolog:message(bad_frettish(ProjName, CompName, ReqName, English)) -->
    [ 'BAD Frettish statement for ~w.~w.~w: ~w~n'
      - [ ProjName, CompName, ReqName, English ] ].

fretment_semantics(Defs, Fretment, Semantics) :-
    Fretment = fretment(scope_info(Scope, ScopeVars),
                        condition_info(Condition, CondVars),
                        component_info(Comp),
                        timing_info(Timing),
                        response_info(Response, RespVars)),
    append([ScopeVars, CondVars, RespVars], AllVars),
    list_to_set(AllVars, Vars),
    SemanticsBase = semantics{ type: "nasa",
                               % The set of variables referenced by this fret
                               % requirement; n.b. referenced by name and not
                               % by the _id.
                               variables: Vars
                             },
    put_dict(SemanticsBase, Scope, Sem1),
    put_dict(Sem1, Condition, Sem2),  % TODO: if no condition, this is null and breaks the next
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
              }, Sem6, Semantics).


req_semantics_defs(Defs, Inp, Semantics) :-
    get_dict(scope, Inp, S),
    get_dict(type, S, ST),
    get_dict(condition, Inp, C),
    get_dict(timing, Inp, T),
    get_dict(response, Inp, Rsp),
    format(atom(DefKeyA), '~w,~w,~w,~w', [ST, C, T, Rsp]),
    get_dict(DefKeyA, Defs, Semantics).


% ----------------------------------------------------------------------

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
sc_mo_tr(null, I, O) :-
    !,
    put_dict(I, _{ scope_mode_pt: "BAD_PT", scope_mode_ft: "BAD_FT" }, O).
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
    ast_to_LTL(PSMV, % PAST,
               PLTL),
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

salt_to_smv(I, SMV) :-   % XXX: test this
    string_chars(I, CS),
    phrase(salt2smv(O), CS, R),
    ( R == []
    -> string_chars(SMV, O)
    ; format('  SMV: ~w~n  REMAINDER: ~w~n', [ SMV, R ]),
      string_chars(SMV, O)
    ).

salt2smv(SMV) --> [ '[', '<', '=' ], number(N), lxm(endSquarePlusOne),
                  salt2smv(Rest),
                  succ(N, M),
                  format(atom(X), '[0,~w]~w~n', [M, Rest]), atom_chars(X, SMV).
salt2smv(SMV) --> [ '[', '<', '=' ], number(N), lxm(endSquare),
                  salt2smv(Rest),
                  format(atom(X), '[0,~w]~w~n', [N, Rest]), atom_chars(X, SMV).
salt2smv(SMV) --> [ '[', '<' ], number(N), lxm(endSquarePlusOne),
                  salt2smv(Rest),
                  format(atom(X), '[0,~w]~w~n', [N, Rest]), atom_chars(X, SMV).
salt2smv(SMV) --> [ '[', '<' ], number(N), lxm(endSquare),
                  salt2smv(Rest),
                  succ(P, N),
                  format(atom(X), '[0,~w]~w~n', [P, Rest]), atom_chars(X, SMV).
salt2smv(SMV) --> [ '[', '=' ], number(N), lxm(endSquarePlusOne),
                  salt2smv(Rest),
                  succ(N, M),
                  format(atom(X), '[~w,~w]~w~n', [M, M, Rest]), atom_chars(X, SMV).
salt2smv(SMV) --> [ '[', '=' ], number(N), lxm(endSquare),
                  salt2smv(Rest),
                  format(atom(X), '[~w,~w]~w~n', [N, N, Rest]), atom_chars(X, SMV).
salt2smv([S|MV]) --> [ S ], salt2smv(MV).
salt2smv([]) --> [].

endSquarePlusOne() --> [ '+', '1', ']' ].
endSquare() --> [ ']' ].


% --------------------

transform_to_AST(I, AST) :-
    writeln(t0),
    subst('=>', '->', I, I1),
    parse_ltl(I1, AST).

ast_to_LTL(I, O) :- emit_ltl(I, O).

ltl_ast_to_CoCo(I, O) :- emit_CoCoSpec(I, O).

% --------------------

xform_past_temporal_unbounded(I, I).  % TODO
xform_future_temporal_unbounded(I, I). % TODO
xform_past_temporal(I, I).  % TODO
xform_future_temporal(I, I). % TODO
xform_past_optimize(I, I).  % TODO xform.transform(X, xform.optimizePT)
xform_future_optimize(I, I).  % TODO xform.transform(X, xform.optimizeFT)

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
