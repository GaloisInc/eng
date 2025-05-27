:- module(lando_fret, [ lando_to_fret/4,
                        scenarios_final_var_name/2
                      ]).

:- use_module(library(http/json)).
:- use_module(datafmts/frettish).
:- use_module(datafmts/fret_json).
:- use_module(datafmts/lando).
:- use_module(datafmts/ltl).
:- use_module(englib).


%% Extract FRET requirements from a Lando SSL (System Specification Language)
% instance.
%
% FretRequirements returns an array of dictionaries where each element is the
% requirement dictionary as:
%     {added_vars:[VDICT],
%      lando_req:{ req_id: ReqID,
%                  req_parent_id: ParID,
%                  req_name: RName,
%                  req_desc: Expl,
%                  req_project: ProjName
%                  fret_req: FRETMENT
%                },
%      requirement:{..FRET json entry..}
%     }
%
%     where VDICT = FRET JSON form of variable =
%                     { _id:,
%                       assignment:,  % KWQ: only if internal; unsupported from lando ...
%                       completed:, copilotAssignment:,  % KWQ: trivial
%                       component_name:,
%                       dataType:,
%                       description:,
%                       idType:,
%                       modeRequirement:, modelComponent:, modeldoc:, modeldoc_id:, moduleName:,  % KWQ: trivial
%                       project:,
%                       variable_name:}
%
%           FMENT = fretment(scope_info({scope:{type:},
%                                        scope_mode:[, exclusive:bool, required:bool]
%                                       }, [SCOPE_VAR_NAMES]),
%                            condition_info({condition:,
%                                            pre_condition:,
%                                            qualifier_word:,
%                                            regular_condition:},[COND_VAR_NAMES]),
%                            component_info({component:},
%                            timing_info({timing:[,duration:|stop_condition:]},
%                                        [TIMING_VAR_NAMES]),
%                            response_info({response:,
%                                           post_condition:},[RESPONSE_VAR_NAMES]),
%
% SrcRefs returns a list of (reqsrc(FRET_REQ_ID, srcref(REQID, REQDOC)))

lando_to_fret(LandoSSL, FretRequirements, FretVariables, SrcRefs) :-
    get_dict(body, LandoSSL, Body),
    get_semantics_defs(SemanticDefs),
    (extract_fret(SemanticDefs, Body, Reqs, Refs, Status)
    -> ( fret_results(SemanticDefs, Body, Status, Reqs,
                      FretRequirements, FretVariables),
         append(Refs, SrcRefs),
         !  %% green cut to not retry now that it has been successfully converted
       )
    ; print_message(error, fret_conversion_failed()), fail
    ).

prolog:message(fret_conversion_failed()) --> [ 'Failed to convert Lando to FRET' ].
prolog:message(fret_conversion_errors(_Cnt)) --> [ 'FRET extractions failed' ].

fret_results(Defs, SSLBody, 0, InpReqs, OutReqs, OutVars) :-
    !,
    collect_vars(Defs, SSLBody, InpReqs, [], OutReqs, OutVars).
fret_results(_, _, Status, _, _, _) :-
    !,
    print_message(error, fret_conversion_errors(Status)),
    fail.

resource(fret_semantics, 'src/semantics.json').

% --------------------

collect_vars(_, _, [], [], [], []).
collect_vars(Defs, SSLBody, [noreq|InpReqs], Vars, OutReqs, OutVars) :-
    !,
    collect_vars(Defs, SSLBody, InpReqs, Vars, OutReqs, OutVars).
collect_vars(Defs, SSLBody, [FretReq|InpReqs], Vars, OutReqs, OutVars) :-
    collect_vars(Defs, SSLBody, InpReqs, Vars, SubReqs, SubVars),
    get_dict(requirement, FretReq, Req),
    get_dict(semantics, Req, Semantics),
    get_dict(variables, Semantics, ReqVars),
    InpConsts = inputs(FretReq, SSLBody),
    collect_req_vars(InpConsts, ReqVars, SubVars, OutFretReq, OutVars),
    OutReqs = [OutFretReq|SubReqs].

collect_req_vars(Inputs, [], VS, FretReq, VS) :- Inputs = inputs(FretReq, _).
collect_req_vars(Inputs, [noreq|RVS], VS, OutReq, OutVS) :-
    !,
    collect_req_vars(Inputs, RVS, VS, OutReq, OutVS).
collect_req_vars(Inputs, [RV|RVS], VS, OutReq, OutVS) :-
    component_var(RV, Inputs, CompVar),
    !,
    get_dict(variable_name, CompVar, VName),
    (existing_var(VName, VS, CV, VSNoV)
    -> add_var_ref(CV, Inputs, CSV), RemVS = VSNoV
    ; add_var_ref(CompVar, Inputs, CSV), RemVS = VS
    ),
    collect_req_vars(Inputs, RVS, [CSV|RemVS], OutReq, OutVS).
collect_req_vars(Inputs, [RV|RVS], VS, OutReq, OutVS) :-
    events_var(RV, Inputs, Var),
    !,
    get_dict(variable_name, Var, VName),
    (existing_var(VName, VS, EV, VSNoV)
    -> add_var_ref(EV, Inputs, SV), RemVS = VSNoV
    ; add_var_ref(Var, Inputs, SV), RemVS = VS
    ),
    collect_req_vars(Inputs, RVS, [SV|RemVS], OutReq, OutVS).
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

    update_req_with_var(Inputs, RV, InitStateV, FinalStateV, UpdInps, AddVars),

    % Update the initial state variable Variable (if necessary) with a reference
    % to this Req.
    % Update the final state variable Variable (if necessary) with a reference
    % to this Req.
    % Add any other added variables
    add_upd_vars(UpdInps, VS, [InitStateV,FinalStateV|AddVars], NextVS),
    % Don't duplicate the state variable
    get_dict(variable_name, MainV, MainVName),
    (existing_var(MainVName, NextVS, _, _)
    ->  collect_req_vars(UpdInps, RVS, NextVS, OutReq, OutVS)
    ; collect_req_vars(UpdInps, RVS, [MainV|NextVS], OutReq, OutVS)
    ).
collect_req_vars(Inputs, [RV|RVS], VS, OutReq, OutVS) :-
    print_message(warning, no_fret_var_info(RV)),
    collect_req_vars(Inputs, RVS, VS, OutReq, OutVS).

add_upd_vars(_, CurVars, [], CurVars).
add_upd_vars(UpdInps, CurVars, [V|VS], UpdVars) :-
    add_upd_var(UpdInps, CurVars, V, NewVars),
    add_upd_vars(UpdInps, NewVars, VS, UpdVars).

add_upd_var(UpdInps, CurVars, V, [UpdV|NewVars]) :-
    get_dict(variable_name, V, N),
    (existing_var(N, CurVars, ExistingV, VSNoV)
    -> add_var_ref(ExistingV, UpdInps, UpdV), NewVars = VSNoV
    ; add_var_ref(V, UpdInps, UpdV), NewVars = CurVars
    ).

existing_var(VName, [Var|VS], Var, VS) :- get_dict(variable_name, Var, VName).
existing_var(VName, [V|Vars], Var, [V|VSNoV]) :-
    existing_var(VName, Vars, Var, VSNoV).

add_var_ref(Var, inputs(FretReq, _), UpdVar) :-
    get_dict(lando_req, FretReq, FR),
    get_dict(req_id, FR, ReqID),
    add_var_ref_(Var, ReqID, UpdVar).
add_var_ref_(Var, ReqID, UpdVar) :-
    (get_dict(reqs, Var, Reqs) ; Reqs = []),
    (member(ReqID, Reqs)
    -> UpdVar = Var
    ; put_dict(_{reqs: [ReqID|Reqs]}, Var, UpdVar)
    ).

update_req_with_var(inputs(FretReq, SSL), RV, InitialSV, FinalSV,
                    inputs(UpdReq, SSL), VarAdds) :-
    get_dict(requirement, FretReq, Req),
    get_dict(lando_req, FretReq, LandoReq),
    % If the user referenced a var in the original, then assume they have
    % responsibility for the proper format, otherwise we add a "SV = " to the RV
    % instances in the fret and then re-parse.
    req_needs_var(FretReq, Req, InitialSV, PreVar),
    req_needs_var(FretReq, Req, FinalSV, PostVar),
    update_req_with_var_(Req, LandoReq, RV, PreVar, PostVar, OutReq, VarAdds),
    (get_dict(added_vars, FretReq, AddedVars) ; AddedVars = []),
    append(AddedVars, VarAdds, VAS),
    ((OutReq == no_update ; OutReq == noreq)
    -> put_dict(_{added_vars:VAS}, FretReq, UpdReq)
    ; put_dict(_{added_vars:VAS}, OutReq, UpdReq)
    ).

update_req_with_var_(_, _, _, hasvar, hasvar, no_update, []) :- !.
update_req_with_var_(Req, LandoReq, RV, hasvar, PostVar, UpdReq, [PostVar|AddVars]) :-
    !,
    get_dict(fulltext, Req, OrigEnglish),
    split_frettish(OrigEnglish, Pre, Post),
    add_var_state_access(Req, RV, PostVar, Post, UpdPost, AddVars),
    update_req_with_newfret(Pre, UpdPost, Req, LandoReq, UpdReq).
update_req_with_var_(Req, LandoReq, RV, PreVar, hasvar, UpdReq, [PreVar|AddVars]) :-
    !,
    get_dict(fulltext, Req, OrigEnglish),
    split_frettish(OrigEnglish, Pre, Post),
    add_var_state_access(Req, RV, PreVar, Pre, UpdPre, AddVars),
    update_req_with_newfret(UpdPre, Post, Req, LandoReq, UpdReq).
update_req_with_var_(Req, LandoReq, RV, PreVar, PostVar, UpdReq, [PreVar, PostVar|AddVars]) :-
    get_dict(fulltext, Req, OrigEnglish),
    split_frettish(OrigEnglish, Pre, Post),
    add_var_state_access(Req, RV, PreVar, Pre, UpdPre, AddVars1),
    add_var_state_access(Req, RV, PostVar, Post, UpdPost, AddVars2),
    append([AddVars1, AddVars2], AddVars),
    update_req_with_newfret(UpdPre, UpdPost, Req, LandoReq, UpdReq).

update_req_with_newfret(UpdPre, UpdPost, Req, LandoReq, UpdReq) :-
    frettish_splitword(SplitWord),
    format(atom(English), '~w~w~w', [ UpdPre, SplitWord, UpdPost ]),
    get_dict(reqid, Req, RName),
    format(atom(Context), 'Add state references into FRET req ~w~n', [RName]),
    !,
    parse_fret_into_req(Context, Req, LandoReq, English, UpdReq).

split_frettish(Frettish, PreCond, PostCond) :-
    string_chars(Frettish, CS),
    frettish_splitword(SplitWord),
    string_chars(SplitWord, SS),
    append(PreCondCS, SS, AShall),
    append(AShall, PostCondCS, CS),
    string_chars(PreCond, PreCondCS),
    string_chars(PostCond, PostCondCS),
    !.

frettish_splitword(" satisfy "). % timing uses input variables
% frettish_splitword(" shall ").  % timing uses output variables


req_needs_var(FretReq, _, Var, Var) :- get_dict(added_vars, FretReq, Vars),
                                       member(Var, Vars), !.
req_needs_var(_, Req, Var, hasvar) :- req_accesses_var(Req, Var), !.
req_needs_var(_, _, Var, Var).

req_accesses_var(Req, Var) :-
    get_dict(semantics, Req, Semantics),
    get_dict(variables, Semantics, ReqVars),
    get_dict(variable_name, Var, VName),
    member(VName, ReqVars).

add_var_state_access(Req, LclVName, StateVar, Phrase, Out, AddVars) :-
    % Note: this does a crude search-and-replace in the text; if the frettish was
    % parsed to an API this could be much more refined.
    get_dict(variable_name, StateVar, N),
    format(atom(Repl), '(~w = ~w)', [N, LclVName]),
    subst(LclVName, Repl, Phrase, NewPhrase),
    fix_scope_(Req, Repl, N, LclVName, NewPhrase, Out, AddVars).

% More crude search-and-replace: the Scope portion of a frettish statement
% can only refer to a "mode" by name, whereas this just did a (statevar =
% foo) subsitutution.  Handle this here by replacing scope prefixes with a
% reference to a local variable which will be created to handle this.
fix_scope_(Req, Repl, StateVName, LclVName, NewPhrase, Out, [Var]) :-
    member(ScopeWord, ["In", "in",
                       "Not in", "not in",
                       "Only in", "only in",
                       "Before", "before",
                       "Only before", "only before",
                       "After", "after",
                       "Only after", "only after"
                      ]),
    member(MatchFmt, ['~w ~w ', '~w mode ~w ']),
    format(atom(Match), MatchFmt, [ScopeWord, Repl]),
    atom_string(Match, MatchS),
    format(atom(VarName), '~w__mode', [LclVName]),
    format(atom(Fixed), '~w ~w ', [ScopeWord, VarName]),
    string_concat(MatchS, _, NewPhrase),
    subst(MatchS, Fixed, NewPhrase, ModePhrase),
    with_stateref(StateVName, LclVName, ModePhrase, Out),
    get_dict(project, Req, ProjName),
    get_dict(semantics, Req, Semantics),
    get_dict(component, Semantics, CompName),
    mkVar(ProjName, CompName, VarName,
          "scope state value reference", "boolean", "Internal", Repl, Var).
fix_scope_(_, _, _, _, Out, Out, []).

with_stateref(StateVName, _, OutPhrase, OutPhrase) :-
    string_codes(StateVName, SC),
    string_codes(OutPhrase, MC),
    append(_, SVP, MC),
    append(SC, _, SVP),
    % already has a reference to StateVName, no modification needed.
    !.
with_stateref(StateVName, LclVName, ModePhrase, OutPhrase) :-
    % Add a no-op reference to the state variable so that it will be added as an
    % input to the kind2 Lustre node.
    split_string(ModePhrase, " ", "", Words),
    (reverse(Words, [Comp, "the" |Rest]) ; reverse(Words, [Comp|Rest])),
    reverse(Rest, P),
    intercalate(P, " ", PS),
    format(atom(OutPhrase), '~w & (~w = ~w) the ~w',
           [ PS, StateVName, LclVName, Comp ]),
    !.


% ----------

% Search the Lando elements (recursively) to find a "component" declaration for
% this name with the expected sub-elements.

component_var(VName, inputs(_, SSLBody), CompVar) :-
    find_specElement(SSLBody, lando_fret:component_var_match(VName),
                     found(ProjName, CompName, SpecElement)),
    ( fret_usage_type(SpecElement, Usage, Type, ModeReqs)
    -> get_dict(explanation, SpecElement, Expl),
       mkVar(ProjName, CompName, VName, Expl, Type, Usage, ModeReqs, CompVar)
    ; print_message(warning, no_fret_var_info(component, VName)),
      fail
    ).
component_var_match(VName, E) :-
    is_dict(E, component),
    specElement_ref(E, VName).


prolog:message(no_fret_var_info(ElementType, VarName)) -->
    [ 'Found ~w for ~w but there is no FRET specification'
      - [ ElementType, VarName ] ].
prolog:message(no_fret_var_info(VarName)) -->
    [ 'No FRET information for variable ~w' - [ VarName ] ].

% ----------

% Search the Lando elements (recursively) to find a "scenarios" declaration for
% which this input name is the scenario's main name (in either initial or final
% name).

scenarios_var(VName, inputs(_, SSLBody), Var) :-
    find_specElement(SSLBody, lando_fret:scenarios_var_match(VName),
                     found(ProjName, CompName, SpecElement)),
    scenarios_var_name(SpecElement, VName, Usage),
    string_concat("scenarios state ", Usage, Expl),
    mkVar(ProjName, CompName, VName, Expl, "integer", Usage, "", Var).

scenarios_var_match(VName, E) :- is_dict(E, scenarios),
                                 scenarios_var_name(E, VName, _).

scenarios_var_name(SpecElement, VName, "Input") :-
    get_dict(name, SpecElement, ScenariosName),
    string_concat(VName, " Values", ScenariosName).
scenarios_var_name(SpecElement, VName, "Output") :-
    get_dict(name, SpecElement, ScenariosName),
    string_concat(SName, " Values", ScenariosName),
    scenarios_final_var_name(SName, VName).

scenarios_final_var_name(InitName, FinalName) :-
    string_concat(InitName, "__NXT", FinalName).

% ----------

% Search the Lando elements (recursively) to find a "scenarios" declaration for
% which this input name is one of the scenario names.

scenario_var(VName, inputs(_, SSLBody), MainVar, ScenarioVar) :-
    find_specElement(SSLBody, lando_fret:scenario_var_name(VName),
                     found(ProjName, CompName, SpecElement)),
    get_dict(name, SpecElement, ScenarioName),
    string_concat(ScenarioVarName, " Values", ScenarioName),
    get_dict(scenarios, SpecElement, Scenarios),
    find_scenario_var(VName, Scenarios, 0, Desc, Value),
    scenarios_final_var_name(ScenarioVarName, FinalVarName),
    format(atom(VString), '~w ', [Value]), % don't output numerics, only strings
    % n.b. deduplication is handled elsewhere
    mkVar(ProjName, CompName, VName, Desc, "integer", "Internal", VString, MainVar),
    mkVar(ProjName, CompName, ScenarioVarName, "Initial state", "integer", "Input", "", InitialVar),
    mkVar(ProjName, CompName, FinalVarName, "Final state", "integer", "Output", "", FinalVar),
    ScenarioVar = scenario(InitialVar, FinalVar, internal_transition).

scenario_var_name(VName, E) :-
    is_dict(E, scenarios),
    get_dict(scenarios, E, Scenarios),
    find_scenario_var(VName, Scenarios, 0, _, _).

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

events_var(VName, inputs(_, SSLBody), Var) :-
    find_specElement(SSLBody, lando_fret:events_var_name(VName),
                     found(ProjName, CompName, SpecElement)),
    get_dict(events, SpecElement, Events),
    find_event_var(VName, Events, Desc),
    % n.b. deduplication is handled elsewhere
    mkVar(ProjName, CompName, VName, Desc, "boolean", "Input", "", Var).

events_var_name(VName, E) :-
    is_dict(E, events),
    get_dict(events, E, Events),
    find_event_var(VName, Events, _).

find_event_var(VName, [Event|_], Desc) :-
    get_dict(id, Event, VName),
    !,
    get_dict(text, Event, Desc).
find_event_var(VName, [_|Events], Desc) :-
    find_event_var(VName, Events, Desc).


% ----------------------------------------------------------------------

get_semantics_defs(Defs) :-
    open('res://lando_fret:fret_semantics', read, Strm),
    json_read_dict(Strm, Defs),
    !,  % following will backtrack, so don't allow backtracking beyond here
    define_fret_semantics(Defs).
define_fret_semantics(D) :-
    get_dict(Key, D, Semantics),
    split_string(Key, ",", "", [ScopeType,Cond,Timing,Response]),
    atom_string(ST, ScopeType),
    atom_string(C, Cond),
    atom_string(T, Timing),
    atom_string(Rsp, Response),
    assertz(fret_semantics(ST, C, T, Rsp, Semantics)),
    fail.  % backtrack to next get_dict(Key, ...)
define_fret_semantics(_).  % pass after all backtracking done

:- dynamic lando_fret:fret_semantics/5.

extract_fret(Defs, SSLBody, Reqs, Refs, Status) :-
    findall(F, find_specElement(SSLBody, lando_fret:is_lando_req, F), Found),
    lando_reqs_to_fret_reqs(Defs, Found, Reqs, Refs, Status).

% Create FRET requirements from FRET: indexing statements in Lando requirements
lando_reqs_to_fret_reqs(_, [], [], [], 0).
lando_reqs_to_fret_reqs(Defs, [found(ProjName, CompName, SpecElement)|Fnd],
                        Reqs, ReqRefs, Status) :-
    specElement_ref(SpecElement, Name),
    get_dict(explanation, SpecElement, Expl),
    get_dict(uid, SpecElement, UID),
    get_dict(indexing, SpecElement, Indexing),

    make_fret_reqs(Defs, ProjName, CompName, Name, Expl, UID,
                   0, Indexing, ThisReqs, Sts),

    % Create a mapping from FRET requirements back to the originating source
    % requirements and documents (if specified via "indexing source: ID from DOC"
    % Lando entries).
    findall(S, source_ref(Indexing, S), SRefs),
    maplist(mkReqRef(SRefs), ThisReqs, ThisReqRefs),

    lando_reqs_to_fret_reqs(Defs, Fnd, NextReqs, NextReqRefs, NextSts),
    prepend_valid_reqs(ThisReqs, NextReqs, Reqs),
    append([ThisReqRefs, NextReqRefs], ReqRefs),
    Status is Sts + NextSts.

is_lando_req(E) :- is_dict(E, requirement).

mkReqRef([], _, []).
mkReqRef(_, noreq, []) :- !.
mkReqRef(SRefs, Req, ReqRefs) :-
    get_dict(lando_req, Req, LR),
    get_dict(req_id, LR, RID),
    mkReqRef_(SRefs, RID, ReqRefs).
mkReqRef_([], _, []).
mkReqRef_([S|SR], RID, [reqsrc(RID, S)|RSRS]) :-
    mkReqRef_(SR, RID, RSRS).

source_ref(Indexing, srcref(ReqTag, ReqSrc)) :-
    member(E, Indexing),
    get_dict(key, E, "source"),
    get_dict(values, E, Vals),
    member(V, Vals),
    string_concat(ReqTag, FromSfx, V),
    string_concat(" from ", ReqSrc, FromSfx).

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
    (parse_fret_or_error(ProjName, CompName, ReqName, Expl, UID, Num,
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
make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID,
               Num, [_|IXS], Reqs, Sts) :-
    make_fret_reqs(Defs, ProjName, CompName, ReqName, Expl, UID,
                   Num, IXS, Reqs, Sts).


parse_fret_or_error(ProjName, CompName, ReqName, Expl, UID, Num, Index, Req) :-
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
    LandoReqBase = lando_requirement{ req_id: ReqID,
                                      req_parent_id: ParID,
                                      req_name: RName,
                                      req_desc: Expl,
                                      req_project: ProjName
                                    },
    ReqBase = requirement{ reqid: RName,
                           parent_reqid: ParID,
                           project: ProjName,
                           rationale: Expl,
                           status: "",
                           comments: "",
                           '_id': ReqID
                         },
    !,
    parse_fret_into_req(Context, ReqBase, LandoReqBase, English, Req).
parse_fret_into_req(Context, ReqBase, LandoReqBase, English, Req) :-
    parse_fret(Context, English, FretMent),
    !,
    ( fretment_semantics(FretMent,
                         ranges{ scopeTextRange:[0, 1],
                                 conditionTextRange:[2, 3],
                                 componentTextRange:[4, 5],
                                 timingTextRange:[6, 7],
                                 responseTextRange:[8, 9]
                               },
                         FretReq), !
    -> put_dict(_{fulltext: English, semantics: FretReq}, ReqBase, Requirement),
       put_dict(_{fret_req: FretMent}, LandoReqBase, LandoReq),
       Req = _{requirement:Requirement,  % KWQ: deprecated
               lando_req:LandoReq}
    ; print_message(error, no_semantics(Context, English, FretMent)), fail
    ).
parse_fret_into_req(Context, _, English, noreq) :-
    print_message(error, bad_frettish(Context, English)).


prolog:message(no_semantics(Context, English, FretMent)) -->
    [ 'Unable to determine semantics for ~w: ~w~n   :: ~w~n'
      - [ Context, English, FretMent ] ].
prolog:message(bad_frettish(Context, English)) -->
    [ 'BAD Frettish statement for ~w: ~w~n'
      - [ Context, English ] ].

fretment_semantics(Fretment, Ranges, Semantics) :-
    fretment_to_fretsemantics(Fretment, Ranges, Semantics).

% --------------------

xform_past_temporal_unbounded(I, I).  % TODO (only for scope mode)
xform_future_temporal_unbounded(I, I). % TODO

