:- module(lando_fret, [ lando_to_fret/3 ]).

:- use_module(library(http/json)).
:- use_module('datafmts/frettish').
:- use_module('datafmts/lando').
:- use_module('datafmts/ltl').
:- use_module('englib').


lando_to_fret(LandoSSL, FretRequirements, FretMents) :-
    get_dict(body, LandoSSL, Body),
    get_semantics_defs(SemanticDefs),
    (phrase(extract_fret("Default", "Default", SemanticDefs, Reqs, _, _, Status),
            Body, Remaining)
    -> ( fret_results(SemanticDefs, Body, Remaining, Status, Reqs,
                      FretRequirements, FretVariables),
         fretOut(FretRequirements, OutRequirements),
         FretMents = fret{ requirements: OutRequirements,
                           variables: FretVariables
                         }
       )
    ; print_message(error, fret_conversion_failed()), fail
    ).

prolog:message(fret_conversion_failed()) --> [ 'Failed to convert Lando to FRET' ].
prolog:message(fret_conversion_errors(_Cnt)) --> [ 'FRET extractions failed' ].
prolog:message(fret_extra(Extra)) -->
    { length(Extra, L) },
    [ 'Unexpected extra ~w not converted to Fret: ~w' - [L, Extra] ].

fret_results(Defs, SSLBody, [], 0, InpReqs, OutReqs, OutVars) :-
    !,
    collect_vars(Defs, SSLBody, InpReqs, [], OutReqs, OutVars).
fret_results(_, _, [], Status, _, _, _) :-
    !,
    print_message(error, fret_conversion_errors(Status)),
    fail.
fret_results(_, _, Remaining, _, _, _, _) :-
    !,
    print_message(error, fret_extra(Remaining)),
    fail.

fretOut([], []).
fretOut([FullReq|FReqs], [R|RS]) :-
    get_dict(requirement, FullReq, R),
    fretOut(FReqs, RS).

resource(fret_semantics, 'src/semantics.json').

% --------------------

collect_vars(_, _, [], [], [], []).
collect_vars(Defs, SSLBody, [FretReq|InpReqs], Vars, OutReqs, OutVars) :-
    collect_vars(Defs, SSLBody, InpReqs, Vars, SubReqs, SubVars),
    get_dict(requirement, FretReq, Req),
    get_dict(semantics, Req, Semantics),
    get_dict(variables, Semantics, ReqVars),
    InpConsts = inputs(Defs, FretReq, SSLBody),
    collect_req_vars(InpConsts, ReqVars, SubVars, OutFretReq, OutVars),
    OutReqs = [OutFretReq|SubReqs].

collect_req_vars(Inputs, [], VS, FretReq, VS) :- Inputs = inputs(_, FretReq, _).
collect_req_vars(Inputs, [RV|RVS], VS, OutReq, OutVS) :-
    component_var(RV, Inputs, CompVar),
    !,
    add_var_ref(CompVar, Inputs, V),
    collect_req_vars(Inputs, RVS, [V|VS], OutReq, OutVS).
collect_req_vars(Inputs, [RV|RVS], VS, OutReq, OutVS) :-
    events_var(RV, Inputs, Var),
    !,
    add_var_ref(Var, Inputs, V),
    collect_req_vars(Inputs, RVS, [V|VS], OutReq, OutVS).
collect_req_vars(Inputs, [RV|RVS], VS, OutReq, OutVS) :-
    scenarios_var(RV, Inputs, Var),
    !,
    add_var_ref(Var, Inputs, V),
    collect_req_vars(Inputs, RVS, [V|VS], OutReq, OutVS).
collect_req_vars(Inputs, [RV|RVS], VS, OutReq, OutVS) :-
    scenario_var(RV, Inputs, MainV,
                 scenario(InitStateV, FinalStateV, internal_transition)), % ScenarioV,
    % ScenarioT)), The initial state variable uses the "scenarios" declared name;
    % the FinalStateV name is derived from that. Thus it is only necessary to
    % check InitStateV's name.
    get_dict(variable_name, InitStateV, SVName),
    \+ component_var(SVName, Inputs, _),
    % Verify there's no explicit "component" declaration of this MainV
    \+ component_var(RV, Inputs, _),
    !,
    % The RV references a specific scenario (RV/MainV) in a scenarios group
    % (SVName), and there is no component declaration for RV or ScenarioV, so the
    % ScenarioV is a PAIR of integer state variables (one for initial, one for
    % final), where one of the values is RV, and RV is an internal variable
    % enumeration value.  The output FRET requirement will need to reference the
    % ScenarioV in the Scope, Conditions, or Responses portion (if it doesn't
    % already) as well as cross-referencing the ScenarioV and the Req to each
    % other in addition to cross referencing the MainV and the Req together.

    % Update the Req to ensure it references the Scenario state initial and final
    % variables--if needed--by modifying the FRETtish English and re-parsing.
    update_req_with_var(Inputs, RV, InitStateV, FinalStateV, UpdInps),

    % Update the initial state variable Variable (if necessary) with a reference
    % to this Req.
    (existing_var(SVName, VS, SV, VSNoV)
    -> add_var_ref(SV, UpdInps, ISV), MidVS = VSNoV
    ; add_var_ref(InitStateV, UpdInps, ISV), MidVS = VS
    ),
    % Update the final state variable Variable (if necessary) with a reference
    % to this Req.
    get_dict(variable_name, FinalStateV, FSVName),
    (existing_var(FSVName, MidVS, ExistingFSV, FVSNoV)
    -> add_var_ref(ExistingFSV, UpdInps, FSV), NextVS = FVSNoV
    ; add_var_ref(FinalStateV, UpdInps, FSV), NextVS = MidVS
    ),
    % Don't duplicate the state variable
    get_dict(variable_name, MainV, MainVName),
    (existing_var(MainVName, NextVS, _, _)
    ->  collect_req_vars(UpdInps, RVS, [ISV,FSV|NextVS], OutReq, OutVS)
    ; collect_req_vars(UpdInps, RVS, [MainV,ISV,FSV|NextVS], OutReq, OutVS)
    ).
collect_req_vars(Inputs, [RV|RVS], VS, OutReq, OutVS) :-
    print_message(warning, no_fret_var_info(RV)),
    collect_req_vars(Inputs, RVS, VS, OutReq, OutVS).

existing_var(VName, [Var|VS], Var, VS) :- get_dict(variable_name, Var, VName).
existing_var(VName, [V|Vars], Var, [V|VSNoV]) :-
    existing_var(VName, Vars, Var, VSNoV).

add_var_ref(Var, inputs(_, FretReq, _), UpdVar) :-
    get_dict(requirement, FretReq, Req),
    get_dict('_id', Req, ReqID),
    add_var_ref_(Var, ReqID, UpdVar).
add_var_ref_(Var, ReqID, UpdVar) :-
    (get_dict(reqs, Var, Reqs) ; Reqs = []),
    (member(ReqID, Reqs)
    -> UpdVar = Var
    ; put_dict(_{reqs: [ReqID|Reqs]}, Var, UpdVar)
    ).

update_req_with_var(inputs(Defs, FretReq, SSL), RV, InitialSV, FinalSV,
                    inputs(Defs, UpdReq, SSL)) :-
    get_dict(requirement, FretReq, Req),
    req_needs_var(Req, InitialSV, PreVar),
    req_needs_var(Req, FinalSV, PostVar),
    update_req_with_var_(Defs, Req, RV, PreVar, PostVar, UpdReq).

update_req_with_var_(_, Req, _, hasvar, hasvar, Req) :- !.
update_req_with_var_(Defs, Req, RV, hasvar, PostVar, UpdReq) :-
    !,
    get_dict(fulltext, Req, OrigEnglish),
    split_frettish(OrigEnglish, Pre, Post),
    add_var_state_access(RV, PostVar, Post, UpdPost),
    update_req_with_newfret(Defs, Pre, UpdPost, Req, UpdReq).
update_req_with_var_(Defs, Req, RV, PreVar, hasvar, UpdReq) :-
    !,
    get_dict(fulltext, Req, OrigEnglish),
    split_frettish(OrigEnglish, Pre, Post),
    add_var_state_access(RV, PreVar, Pre, UpdPre),
    update_req_with_newfret(Defs, UpdPre, Post, Req, UpdReq).
update_req_with_var_(Defs, Req, RV, PreVar, PostVar, UpdReq) :-
    get_dict(fulltext, Req, OrigEnglish),
    split_frettish(OrigEnglish, Pre, Post),
    add_var_state_access(RV, PreVar, Pre, UpdPre),
    add_var_state_access(RV, PostVar, Post, UpdPost),
    update_req_with_newfret(Defs, UpdPre, UpdPost, Req, UpdReq).

update_req_with_newfret(Defs, UpdPre, UpdPost, Req, UpdReq) :-
    format(atom(English), '~w shall ~w', [ UpdPre, UpdPost ]),
    get_dict(reqid, Req, RName),
    format(atom(Context), 'Add state references into FRET req ~w~n', [RName]),
    !,
    parse_fret_into_req(Defs, Context, Req, English, UpdReq).

split_frettish(Frettish, PreCond, PostCond) :-
    string_chars(Frettish, CS),
    string_chars(" shall ", SS),
    append(PreCondCS, SS, AShall),
    append(AShall, PostCondCS, CS),
    string_chars(PreCond, PreCondCS),
    string_chars(PostCond, PostCondCS).

req_needs_var(Req, Var, hasvar) :- req_accesses_var(Req, Var), !.
req_needs_var(_, Var, Var).

req_accesses_var(Req, Var) :-
    get_dict(semantics, Req, Semantics),
    get_dict(variables, Semantics, ReqVars),
    get_dict(variable_name, Var, VName),
    member(VName, ReqVars).

add_var_state_access(LclVName, StateVar, Phrase, Out) :-
    % Note: this does a crude search-and-replace in the text; if the frettish was
    % parsed to an API this could be much more refined.
    get_dict(variable_name, StateVar, N),
    format(atom(Repl), '(~w = ~w)', [N, LclVName]),
    subst(LclVName, Repl, Phrase, Out).

% ----------

% Search the Lando elements (recursively) to find a "component" declaration for
% this name with the expected sub-elements.

component_var(VName, inputs(_, _, SSLBody), CompVar) :-
    phrase(extract_fret_component_var("Default", "Default", VName, CompVar), SSLBody, _).

extract_fret_component_var(ProjName, _, VName, CompVar) -->
    [ SpecElement ],
    { is_dict(SpecElement, SpecType),
      member(SpecType, [ system, subsystem ]), %%%% <- selector
      specElement_ref(SpecElement, SysName),
      get_dict(body, SpecElement, SysBody),
      (ProjName == "Default" -> NewProjName = SysName ; NewProjName = ProjName),
      phrase(extract_fret_component_var(NewProjName, SysName, VName, CompVar), SysBody, _Remaining)
    }.
extract_fret_component_var(ProjName, CompName, VName, Var) -->
    [ SpecElement ],
    { is_dict(SpecElement, component), %%%% <- selector
      specElement_ref(SpecElement, VName),
      ( fret_usage_type(SpecElement, Usage, Type, ModeReqs)
      -> get_dict(explanation, SpecElement, Expl),
         mkVar(ProjName, CompName, VName, Expl, Type, Usage, ModeReqs, Var)
      ; print_message(warning, no_fret_var_info(component, VName)),
        fail
      )
    }.
extract_fret_component_var(ProjName, CompName, VName, Var) -->
    [ _ ],
    extract_fret_component_var(ProjName, CompName, VName, Var).

prolog:message(no_fret_var_info(ElementType, VarName)) -->
    [ 'Found ~w for ~w but there is no FRET specification'
      - [ ElementType, VarName ] ].
prolog:message(no_fret_var_info(VarName)) -->
    [ 'No FRET information for variable ~w' - [ VarName ] ].

% ----------

% Search the Lando elements (recursively) to find a "scenarios" declaration for
% which this input name is the scenario's main name (in either initial or final
% name).

scenarios_var(VName, inputs(_, _, SSLBody), Var) :-
    phrase(extract_fret_scenarios_var("Default", "Default", VName, Var), SSLBody, _).

extract_fret_scenarios_var(ProjName, _, VName, Var) -->
    [ SpecElement ],
    { is_dict(SpecElement, SpecType),
      member(SpecType, [ system, subsystem ]), %%%% <- selector
      specElement_ref(SpecElement, SysName),
      get_dict(body, SpecElement, SysBody),
      (ProjName == "Default" -> NewProjName = SysName ; NewProjName = ProjName),
      phrase(extract_fret_scenarios_var(NewProjName, SysName, VName, Var), SysBody, _Remaining)
    }.
extract_fret_scenarios_var(ProjName, CompName, VName, Var) -->
    [ SpecElement ],
    { is_dict(SpecElement, scenarios), %%%% <- selector
      scenarios_var_name(SpecElement, VName, Usage),
      string_concat("scenarios state ", Usage, Expl),
      mkVar(ProjName, CompName, VName, Expl, "integer", Usage, "", Var)
    }.

extract_fret_scenarios_var(ProjName, CompName, VName, Var) -->
    [ _ ],
    extract_fret_scenarios_var(ProjName, CompName, VName, Var).

scenarios_var_name(SpecElement, VName, "Input") :-
    get_dict(name, SpecElement, ScenariosName),
    string_concat(VName, " Values", ScenariosName).
scenarios_var_name(SpecElement, VName, "Output") :-
    get_dict(name, SpecElement, ScenariosName),
    string_concat(SName, " Values", ScenariosName),
    scenarios_final_var_name(SName, VName).

scenarios_final_var_name(InitName, FinalName) :-
    string_concat(InitName, "_final", FinalName).

% ----------

% Search the Lando elements (recursively) to find a "scenarios" declaration for
% which this input name is one of the scenario names.

scenario_var(VName, inputs(_, _, SSLBody), MainVar, ScenarioVar) :-
    phrase(extract_fret_scenario_var("Default", "Default", VName,
                                     MainVar, ScenarioVar),
           SSLBody, _).

extract_fret_scenario_var(ProjName, _, VName, MainVar, ScenarioVar) -->
    [ SpecElement ],
    { is_dict(SpecElement, SpecType),
      member(SpecType, [ system, subsystem ]), %%%% <- selector
      specElement_ref(SpecElement, SysName),
      get_dict(body, SpecElement, SysBody),
      (ProjName == "Default" -> NewProjName = SysName ; NewProjName = ProjName),
      phrase(extract_fret_scenario_var(NewProjName, SysName, VName, MainVar, ScenarioVar), SysBody, _Remaining)
    }.
extract_fret_scenario_var(ProjName, CompName, VName, MainVar,
                          scenario(InitialVar, FinalVar, internal_transition)) -->
    [ SpecElement ],
    { is_dict(SpecElement, scenarios), %%%% <- selector
      get_dict(name, SpecElement, ScenarioName),
      string_concat(ScenarioVarName, " Values", ScenarioName), %%%% <- selector
      get_dict(scenarios, SpecElement, Scenarios),
      find_scenario_var(VName, Scenarios, 0, Desc, Value),
      scenarios_final_var_name(ScenarioVarName, FinalVarName),
      format(atom(VString), '~w ', [Value]), % don't output numerics, only strings
      % n.b. deduplication is handled elsewhere
      mkVar(ProjName, CompName, VName, Desc, "integer", "Internal", VString, MainVar),
      mkVar(ProjName, CompName, ScenarioVarName, "Initial state", "integer", "Input", "", InitialVar),
      mkVar(ProjName, CompName, FinalVarName, "Final state", "integer", "Output", "", FinalVar)
    }.
extract_fret_scenario_var(ProjName, CompName, VName, MainVar, scenario(Var, mode)) -->  %% Old
    [ SpecElement ],
    { is_dict(SpecElement, scenarios), %%%% <- selector
      get_dict(name, SpecElement, ScenarioName),
      string_concat(ScenarioVarName, " Modes", ScenarioName), %%%% <- selector
      % !,
      get_dict(scenarios, SpecElement, Scenarios),
      find_scenario_var(VName, Scenarios, 0, Desc, Value),
      Expl = "State variable",
      format(atom(F), "~w = ~w", [ScenarioVarName, Value]),
      mkVar(ProjName, CompName, VName, Desc, "boolean", "Mode", F, MainVar),
      mkVar(ProjName, CompName, ScenarioVarName, Expl, "integer", "Output", "", Var)
    }.
extract_fret_scenario_var(ProjName, CompName, VName, MainVar, ScenarioVar) -->
    [ _ ],
    extract_fret_scenario_var(ProjName, CompName, VName, MainVar, ScenarioVar).

find_scenario_var(VName, [Scenario|_], Value, Desc, Value) :-
    get_dict(id, Scenario, VName),
    !,
    get_dict(text, Scenario, Desc).
find_scenario_var(VName, [_|Scenarios], ThisValue, Desc, Value) :-
    succ(ThisValue, NextValue),
    find_scenario_var(VName, Scenarios, NextValue, Desc, Value).


% ----------

% Search the Lando elements (recursively) to find an "events" declaration for
% which this input name is one of the event names.

events_var(VName, inputs(_, _, SSLBody), Var) :-
    phrase(extract_fret_events_var("Default", "Default", VName, Var), SSLBody, _).

extract_fret_events_var(ProjName, _, VName, Var) -->
    [ SpecElement ],
    { is_dict(SpecElement, SpecType),
      member(SpecType, [ system, subsystem ]), %%%% <- selector
      specElement_ref(SpecElement, SysName),
      get_dict(body, SpecElement, SysBody),
      (ProjName == "Default" -> NewProjName = SysName ; NewProjName = ProjName),
      phrase(extract_fret_events_var(NewProjName, SysName, VName, Var), SysBody, _Remaining)
    }.
extract_fret_events_var(ProjName, CompName, VName, Var) -->
    [ SpecElement ],
    { is_dict(SpecElement, events), %%%% <- selector
      get_dict(events, SpecElement, Events),
      find_event_var(VName, Events, Desc),
      % n.b. deduplication is handled elsewhere
      mkVar(ProjName, CompName, VName, Desc, "boolean", "Input", "", Var)
    }.
extract_fret_events_var(ProjName, CompName, VName, Var) -->
    [ _ ],
    extract_fret_events_var(ProjName, CompName, VName, Var).

find_event_var(VName, [Event|_], Desc) :-
    get_dict(id, Event, VName),
    !,
    get_dict(text, Event, Desc).
find_event_var(VName, [_|Events], Desc) :-
    find_event_var(VName, Events, Desc).


% ----------------------------------------------------------------------

get_semantics_defs(Defs) :-
    open('res://lando_fret:fret_semantics', read, Strm),
    json_read_dict(Strm, Defs).

extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, Modes, Status) -->
    [ SpecElement ],
    { is_dict(SpecElement, SpecType),
      member(SpecType, [ system, subsystem ]) %%%% <- selector
    },
    !,
    { specElement_ref(SpecElement, SysName),
      get_dict(body, SpecElement, SysBody),
      (ProjName == "Default" -> NewProjName = SysName ; NewProjName = ProjName),
      phrase(extract_fret(NewProjName, SysName, Defs,
                          SysReqs, SysVars, SysModes, Sts), SysBody, Remaining)
    },
    extract_fret(ProjName, FretCompName, Defs,
                 NextReqs, NextVars, NextModes, NextSts),
    { append(SysReqs, NextReqs, Reqs),
      append(SysVars, NextVars, Vars),  % KWQ TODO normalize vars
      append(SysModes, NextModes, Modes),
      length(Remaining, UnExtractedCnt),
      Status is Sts + NextSts + UnExtractedCnt
    }.
extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, Modes, Status) -->
    [ SpecElement ],
    { is_dict(SpecElement, requirement) }, %%%% <- selector
    !,
    { specElement_ref(SpecElement, Name),
      get_dict(explanation, SpecElement, Expl),
      get_dict(uid, SpecElement, UID),
      get_dict(indexing, SpecElement, Indexing),
      make_fret_reqs(Defs, ProjName, FretCompName, Name, Expl, UID,
                     0, Indexing, ThisReqs, Sts)
    },
    extract_fret(ProjName, FretCompName, Defs, NextReqs, Vars, Modes, NextSts),
    { prepend_valid_reqs(ThisReqs, NextReqs, Reqs),
      Status is Sts + NextSts
    }.
extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, Modes, Status) -->
    [ _ ],  % skip other elements
    extract_fret(ProjName, FretCompName, Defs, Reqs, Vars, Modes, Status).
extract_fret(_, _, _, [], [], [], 0) --> [].

prepend_valid_reqs([], Reqs, Reqs).
prepend_valid_reqs([noreq|RS], Reqs, OutReqs) :-
    prepend_valid_reqs(RS, Reqs, OutReqs).
prepend_valid_reqs([R|RS], Reqs, [R|OutRS]) :-
    prepend_valid_reqs(RS, Reqs, OutRS).

mkVar(ProjName, FretCompName, VarName, Expl, Type, Usage, Special, Var) :-
    format(atom(IDA), "~w~w~w", [ ProjName, FretCompName, VarName ]),
    atom_string(IDA, ID),
    ( Usage == "Internal"
    -> MR = "", AV = Special
    ; (Usage == "Mode"
      -> MR = Special, AV = ""
      ; ( Special == ""
        ; print_message(warning, special_usage_unknown(VarName, Usage, Special))
        ),
        MR = "", AV = ""
      )
    ),
    Var = _{ project: ProjName,
             component_name: FretCompName,
             variable_name: VarName,
             dataType: Type,
             idType: Usage,
             description: Expl,
             '_id': ID,
             modeRequirement: MR,
             assignment: AV,
             copilotAssignment: "",
             moduleName: "",
             modeldoc: false,
             modeldoc_id: "",
             modelComponent: "",
             completed: true
           }.

prolog:message(special_usage_unknown(VName, Usage, Val)) -->
    [ 'Unknown ~w usage for special of ~w in variable ~w'
      - [ Usage, Val, VName ] ].

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

make_fret_reqs(_, _, _, _, _, _, _, [], [], 0).
make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID,
               Num, [Index|IXS], [Req|Reqs], Sts) :-
    get_dict(key, Index, "FRET"),
    (parse_fret_or_error(Defs, ProjName, CompName, ReqName, Expl, UID, Num,
                         Index, Req)
    -> Cnt = 0
    ; Cnt = 1
    ),
    succ(Num, NextNum),
    make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID,
                   NextNum, IXS, Reqs, NextSts),
    Sts is Cnt + NextSts.
make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID,
               Num, [_|IXS], Reqs, Sts) :-
    make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID,
                   Num, IXS, Reqs, Sts).


parse_fret_or_error(Defs, ProjName, CompName, ReqName, Expl, UID, Num, Index, Req) :-
    get_dict(values, Index, Lines),
    intercalate(Lines, " ", English),
    get_dict(pos, Index, pos{line:Line, col:_Col}),
    format(atom(Context), 'line ~w (~w.~w.~w #~w)',
           [ Line, ProjName, CompName, ReqName, Num ]),
    format(atom(ParentID), '~w-req-~w', [ ProjName, UID ]),
    (Num == 0
    -> ReqID = ParentID, ParID = "", RName = ReqName
    ; format(atom(ReqID), '~w-~w', [ ParentID, Num ]),
      format(atom(RName), '~w-~w', [ ReqName, Num ]),
      ParID = ParentID
    ),
    ReqBase = requirement{ reqid: RName,
                           parent_reqid: ParID,
                           project: ProjName,
                           rationale: Expl,
                           status: "",
                           comments: "",
                           '_id': ReqID
                         },
    !,
    parse_fret_into_req(Defs, Context, ReqBase, English, Req).
parse_fret_into_req(Defs, Context, ReqBase, English, Req) :-
    parse_fret(Context, English, FretMent),
    !,
    ( fretment_semantics(Defs, FretMent, FretReq), !
    -> put_dict(_{fulltext: English, semantics: FretReq}, ReqBase, Requirement),
       Req = _{requirement:Requirement, fretment:FretMent}
    ; print_message(error, no_semantics(Context, English, FretMent)), fail
    ).
parse_fret_into_req(_, Context, _, English, noreq) :-
    print_message(error, bad_frettish(Context, English)).


prolog:message(no_semantics(Context, English, FretMent)) -->
    [ 'Unable to determine semantics for ~w: ~w~n   :: ~w~n'
      - [ Context, English, FretMent ] ].
prolog:message(bad_frettish(Context, English)) -->
    [ 'BAD Frettish statement for ~w: ~w~n'
      - [ Context, English ] ].

fretment_semantics(Defs, Fretment, Semantics) :-
    Fretment = fretment(scope_info(Scope, ScopeVars),
                        condition_info(Condition, CondVars),
                        component_info(Comp),
                        timing_info(Timing, TimingVars),
                        response_info(Response, RespVars)),
    append([ScopeVars, CondVars, TimingVars, RespVars], AllVars),
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

    (generate_formulae(SemDefs, Sem5, Sem6)
    -> put_dict(_{ diagramVariables: DV,
                   description: Desc
                 }, Sem6, Semantics)
    ; print_message(error, no_formula()), fail
    ).

prolog:message(no_formula()) --> [ 'Could not generate formulae for frettish' ].

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
    (scope_mode_transform(Defs, Inp, O1)
    ; print_message(error, scope_transform_failed()), fail
    ),
    !,
    (regular_condition_transform(Defs, O1, O2)
    ; print_message(error, condition_transform_failed(regular)), fail
    ),
    !,
    (post_condition_transform(Defs, O2, O3)
    ; print_message(error, condition_transform_failed(post)), fail
    ),
    !,
    (stop_condition_transform(Defs, O3, O4)
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

salt2smv(SMV) --> [ '[', '<', '=' ], lxm(numeric, N), lxm(endSquarePlusOne),
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

% --------------------

xform_past_temporal_unbounded(I, I).  % TODO (only for scope mode)
xform_future_temporal_unbounded(I, I). % TODO

xform_past_temporal(I, O) :-  % provided/returns string
    parse_ltl(I, AST),
    fmap(lando_fret:xpt, AST, XAST),
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
