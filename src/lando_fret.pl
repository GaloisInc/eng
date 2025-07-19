:- encoding(utf8).
:- module(lando_fret, [ lando_to_fret/4,
                        scenarios_final_var_name/2
                      ]).

:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(datafmts/frettish).
:- use_module(datafmts/fret_json).
:- use_module(datafmts/lando).
:- use_module(datafmts/ltl).
:- use_module(englib).
:- use_module(exprlang).


%% Extract FRET requirements from a Lando SSL (System Specification Language)
% instance.
%
% FretRequirements returns an array of dictionaries where each element is the
% requirement dictionary as:
%     {lando_req:{ req_id: ReqID,
%                  req_parent_id: ParID,
%                  req_name: RName,
%                  req_desc: Expl,
%                  req_project: ProjName
%                  fret_req: FRETMENT
%                },
%     }
%
% FretVariables returns either constr(_, _, _, _) or a record supporting varname,
% usage, and type as well as other fields.
%
%
%           FMENT = fretment(scope_info({scope:{type:},
%                                        scope_mode:[, exclusive:bool, required:bool]
%                                       }, [SCOPE_VAR_NAMES]),
%                            condition_info({condition:,
%                                            qualifier_word:,
%                                            regular_condition:}),
%                            component_info({component:},
%                            timing_info({timing:[,duration:|stop_condition:]},
%                                        [TIMING_VAR_NAMES]),
%                            response_info({response:,
%                                           post_condition:}),
%
% SrcRefs returns a list of (reqsrc(FRET_REQ_ID, srcref(REQID, REQDOC)))

lando_to_fret(LandoSSL, FretRequirements, FretVariables, SrcRefs) :-
    get_dict(body, LandoSSL, Body),
    get_semantics_defs(_),  %% asserts fret_semantics/5 facts used below
    fretish_expr_langdef(LangDef),
    % n.b. re-define language because there are user var-define type extensions
    % asserted below (assertz(exprlang:type(...))).
    define_language(LangDef, _),
    (extract_fret(Body, Reqs, Refs, Status)
    -> ( fret_results(Body, Status, Reqs, FretRequirements, FretVariables),
         append(Refs, SrcRefs),
         !,  %% green cut to not retry now that it has been successfully converted
         length(FretRequirements, NFret),
         print_message(information, fret_extracted(NFret))
       )
    ; print_message(error, fret_conversion_failed()), fail
    ).

prolog:message(fret_extracted(N)) --> [ 'Extracted ~w FRET requirements' - [N] ].
prolog:message(fret_conversion_failed()) --> [ 'Failed to convert Lando to FRET' ].
prolog:message(fret_conversion_errors(_Cnt)) --> [ 'FRET extractions failed' ].

fret_results(SSLBody, 0, InpReqs, OutReqs, OutVars) :-
    !,
    collect_vars(SSLBody, InpReqs, OutReqs, OutVars).
fret_results(_SSLBody, 0, InpReqs, InpReqs, []) :-
    !.
fret_results(_, Status, _, _, _) :-
    !,
    print_message(error, fret_conversion_errors(Status)),
    fail.

resource(fret_semantics, 'src/semantics.json').

% --------------------

collect_vars(SSLBody, InpReqs, OutReqs, OutVars) :-
    findall(C, component_var(SSLBody, C), CS),
    findall(E, events_var(SSLBody, E), ES),
    findall(S, scenarios_var(SSLBody, S), SS),
    collect_vars(SSLBody, InpReqs, [], OutReqs, UsedVars),
    foldl(collect_var(CS, ES, SS), UsedVars, [], OutVars).

collect_var(_, _, _, VName ⦂ _, Collected, Collected) :-
    already_collected(VName, Collected), !.
collect_var(CS, _, _, VName ⦂ _, Collected, [Var|Collected]) :-
    var_in(CS, VName, Var),
    !.
collect_var(_, ES, _, VName ⦂ _, Collected, [Var|Collected]) :-
    var_in(ES, VName, Var),
    !.
collect_var(_, _, SS, VName ⦂ _, Collected, [Var|Collected]) :-
    var_in(SS, VName, Var),
    !.
collect_var(_, _, SS, VName ⦂ _, Collected, [Var|Collected]) :-
    member(S, SS),
    get_dict(varname, S, VN),
    scenarios_final_var_name(VN, VName),
    get_dict(desc, S, OD),
    format_str(Desc, 'output ~w', [OD]),
    put_dict(_{varname: VName, usage:"Output", desc:Desc}, S, Var),
    !.
collect_var(_, _, SS, VName ⦂ _, Collected, OutCollected) :-
    member(S, SS),
    get_dict(constructors, S, CVS),
    member((VName, Val, Desc), CVS),
    collect_constr(VName, constr(VName, Val, Desc, S), Collected, OutCollected).
collect_var(_, _, _, VName ⦂ VType, _, _) :-
    print_message(error, undefined_variable(VName, VType)), !, fail.

prolog:message(undefined_variable(VName, VType)) -->
    [ 'Referenced undefined variable in FRETish: ~w (type ~w)' - [VName, VType] ].

collect_constr(VName, _, Collected, Collected) :-
    already_collected(VName, Collected), !.
collect_constr(_, Constr, Collected, [Constr|Collected]).


var_in([V|_],  VName, V) :- get_dict(varname, V, VName), !.
var_in([_|VS], VName, V) :- var_in(VS, VName, V).

already_collected(VName, [constr(VName, _, _, _)|_]) :- !.
already_collected(VName, [constr(_, _, _, _)|VS]) :- !, already_collected(VName, VS).
already_collected(VName, [V|_]) :- get_dict(varname, V, VName), !.
already_collected(VName, [_|VS]) :- already_collected(VName, VS).

collect_vars(_, [], [], [], []).
collect_vars(SSLBody, [noreq|InpReqs], Vars, OutReqs, OutVars) :-
    !,
    collect_vars(SSLBody, InpReqs, Vars, OutReqs, OutVars).
collect_vars(SSLBody, [R|InpReqs], Vars, [R|SubReqs], OutVars) :-
    collect_vars(SSLBody, InpReqs, Vars, SubReqs, SubVars),
    collect_vars_from_req(R, ReqVars),
    append([ReqVars, SubVars], OutVars).

collect_vars_from_req(Req, Vars) :-
    get_dict(lando_req, Req, LR),
    get_dict(fret_req, LR, FR),
    fretment_vars(scope, FR, SVars),
    fretment_vars(condition, FR, CVars),
    fretment_vars(timing, FR, TVars),
    fretment_vars(response, FR, RVars),
    append([SVars, CVars, TVars, RVars], AllVars),
    list_to_set(AllVars, Vars).





% ----------

% Search the Lando elements (recursively) to find a "component" declaration for
% this name with the expected sub-elements.

component_var(SSLBody, Info) :-
    find_specElement(SSLBody, lando_fret:component_var_match,
                     found(ProjName, CompName, SpecElement)),
    fret_usage_type(SpecElement, Usage, Type, _ModeReqs),
    % n.b. ModeReqs (Lando constraints) currently unsupported.
    get_dict(explanation, SpecElement, Expl),
    specElement_ref(SpecElement, VName),
    atom_string(AType, Type),
    Info = var{varname: VName,
               usage: Usage,
               type: AType,
               desc: Expl,
               proj: ProjName,
               comp: CompName
              }.

component_var_match(E) :- is_dict(E, component).

% ----------

% Search the Lando elements (recursively) to find a "scenarios" declaration for
% which this input name is the scenario's main name (in either initial or final
% name).

scenarios_var(SSLBody, Info) :-
    find_specElement(SSLBody, lando_fret:scenarios_var_match,
                     found(ProjName, CompName, SpecElement)),
    scenarios_var_name(SpecElement, VName, "Input"),
    string_concat("scenarios state ", "variable", Expl),
    get_dict(scenarios, SpecElement, Scenarios),
    findall((C,V,D), find_scenario_var(C, Scenarios, 0, D, V), Constructors),
    scenarios_type_name(VName, Ty),
    Info = scenario{varname: VName,
                    usage: "Input",
                    type: Ty,
                    desc: Expl,
                    proj: ProjName,
                    comp: CompName,
                    constructors: Constructors
                   }.

scenarios_var_match(E) :- is_dict(E, scenarios).

scenarios_var_name(SpecElement, VName, "Input") :-
    get_dict(name, SpecElement, ScenariosName),
    string_concat(VName, " Values", ScenariosName).
scenarios_var_name(SpecElement, VName, "Output") :-
    get_dict(name, SpecElement, ScenariosName),
    string_concat(SName, " Values", ScenariosName),
    scenarios_final_var_name(SName, VName).

scenarios_final_var_name(InitName, FinalName) :-
    string_concat(InitName, "__NXT", FinalName).

find_scenario_var(VName, [Scenario|_], Value, Desc, Value) :-
    get_dict(id, Scenario, VName),
    get_dict(text, Scenario, Desc).
find_scenario_var(VName, [_|Scenarios], ThisValue, Desc, Value) :-
    succ(ThisValue, NextValue),
    find_scenario_var(VName, Scenarios, NextValue, Desc, Value).


% ----------

% Search the Lando elements (recursively) to find an "events" declaration for
% which this input name is one of the event names.

events_var(SSLBody, Info) :-
    find_specElement(SSLBody, lando_fret:is_events_var,
                     found(ProjName, CompName, SpecElement)),
    get_dict(events, SpecElement, Events),
    member(Event, Events),
    get_dict(id, Event, VName),
    get_dict(text, Event, Desc),
    Info = var{varname: VName,
               usage: "Input",
               type: boolean,
               desc: Desc,
               proj: ProjName,
               comp: CompName
              }.

is_events_var(E) :- is_dict(E, events).

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

extract_fret(SSLBody, Reqs, Refs, Status) :-
    initial_gamma(LangEnv0),
    add_predefined_vars(LangEnv0, SSLBody, LangEnv, EnabledRewrites),
    findall(F, find_specElement(SSLBody, lando_fret:is_lando_req, F), Found),
    (lando_reqs_to_fret_reqs(LangEnv, Found, Reqs, Refs, Status)
    -> maplist(erase, EnabledRewrites)
    ; % Ensure rewrites are removed even on failure to parse lando reqs into fret reqs
      maplist(erase, EnabledRewrites), !, fail
    ).

add_predefined_vars(LangEnv0, SSLBody, LangEnv, Rewrites) :-
    add_component_vars(LangEnv0, SSLBody, LangEnv1),
    add_events_vars(LangEnv1, SSLBody, LangEnv2),
    add_scenario_vars(LangEnv2, SSLBody, LangEnv, Rewrites).

add_component_vars(LangEnv0, SSLBody, LangEnv) :-
    findall(C, component_var(SSLBody, C), CS),
    foldl(add_cvar, CS, LangEnv0, LangEnv).
add_cvar(C, InpEnv, OutEnv) :-
    get_dict(varname, C, Name),
    get_dict(type, C, Type),
    fresh_var(InpEnv, Name, Type, OutEnv).

add_events_vars(LangEnv0, SSLBody, LangEnv) :-
    findall(C, events_var(SSLBody, C), CS),
    foldl(add_evar, CS, LangEnv0, LangEnv).
add_evar(C, InpEnv, OutEnv) :-
    get_dict(varname, C, Name),
    get_dict(type, C, Type),
    fresh_var(InpEnv, Name, Type, OutEnv).

add_scenario_vars(LangEnv0, SSLBody, LangEnv, Rewrites) :-
    findall(S, scenarios_var(SSLBody, S), SS),
    fretish_expr_langdef(LD),
    get_dict(language, LD, Lang),
    foldl(add_svar(Lang), SS, (LangEnv0, []), (LangEnv, Rewrites)).
add_svar(Lang, S, (InpEnv, RW), (OutEnv, [TRef|NewRW])) :-
    get_dict(varname, S, Name),
    scenarios_final_var_name(Name, OutName),
    get_dict(type, S, Type),
    assertz(exprlang:type(Lang, Type), TRef), % this is now a known type for the language
    fresh_var(InpEnv, Name, Type, Env1),
    fresh_var(Env1, OutName, Type, Env2),
    % These are nullary constructors, and aren't technically variables, but
    % tracking them as such allows the exprlang typechecking to validate them,
    % and the fact that they are constants and not variables is only important
    % when emitting them into some other representation.
    get_dict(constructors, S, Constructors),
    foldl([(N,_,_),E,EO]>>fresh_var(E, N, Type, EO), Constructors, Env2, Env3),
    add_rewrites(Lang, Name, OutName, Type, Constructors, Env3, RW, NewRW, OutEnv).
add_rewrites(_, _, _, _, [], Env, RW, RW, Env).
add_rewrites(Lang, CVar, OutCVar, CType, [(CName, _CVal, _CDesc)|CS], Env, RW,
             OutRW, OutEnv) :-
    assertz(exprlang:type_repair(Lang, XEnv,
                                 term(ident(CName), CType), boolean,
                                 op(eq(term(ident(OutCVar), CType),
                                       term(ident(CName), CType)), boolean),
                                 XEnv) :- frettish:in_response,
            RW1),
    assertz(exprlang:type_repair(Lang, XEnv,
                                 term(ident(CName), CType), boolean,
                                 op(eq(term(ident(CVar), CType),
                                       term(ident(CName), CType)), boolean),
                                 XEnv),
            RW2),
    add_rewrites(Lang, CVar, OutCVar, CType, CS, Env, [RW1,RW2|RW], OutRW, OutEnv).

:- dynamic frettish:in_response/0.

% Create FRET requirements from FRET: indexing statements in Lando requirements
lando_reqs_to_fret_reqs(_, [], [], [], 0).
lando_reqs_to_fret_reqs(LangEnv, [found(ProjName, CompName, SpecElement)|Fnd],
                        Reqs, ReqRefs, Status) :-
    specElement_ref(SpecElement, Name),
    get_dict(explanation, SpecElement, Expl),
    get_dict(uid, SpecElement, UID),
    get_dict(indexing, SpecElement, Indexing),

    make_fret_reqs(ProjName, CompName, LangEnv, Name, Expl, UID,
                   0, Indexing, ThisReqs, Sts),

    % Create a mapping from FRET requirements back to the originating source
    % requirements and documents (if specified via "indexing source: ID from DOC"
    % Lando entries).
    findall(S, source_ref(Indexing, S), SRefs),
    maplist(mkReqRef(SRefs), ThisReqs, ThisReqRefs),

    lando_reqs_to_fret_reqs(LangEnv, Fnd, NextReqs, NextReqRefs, NextSts),
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

fret_usage_type(Element, Usage, Type, ModeReqs) :-
    get_dict(features, Element, Features),
    fret_var_usage_feature(Element, Features, Usage, ModeReqs),
    fret_var_type_feature(Features, Type).

fret_var_usage_feature(Element, Features, Usage, ModeReqs) :-
    member(Feature, Features),
    fret_var_usage_(Element, Features, Feature, Usage, ModeReqs).
fret_var_usage_(Element, Features, Feature, Usage, ModeReqs) :-
    get_dict(text, Feature, T),
    string_concat(Before, " var.", T),
    string_concat("FRET ", Usage, Before),
    var_modereqs(Element, Features, Usage, ModeReqs).

var_modereqs(_, Features, "Mode", ModeReqs) :-
    !,
    findall(T, regular_constraints(Features, T), TS),
    conjunction(TS, ModeReqs).
var_modereqs(_, _, "Input", "") :- !.
var_modereqs(_, _, "Output", "") :- !.
var_modereqs(Element, _, Other, "") :-
    !,
    print_message(warning, unknown_mode(Element, Other)),
    fail.

prolog:message(unknown_mode(Element, Mode)) -->
    { specElement_ref(Element, Name) },
    [ 'Ignoring ~w: unknown mode ~w' - [ Name, Mode ] ].

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

make_fret_reqs(_, _, _, _, _, _, _, [], [], 0).  % end of list
make_fret_reqs(ProjName, CompName, LangEnv, ReqName, Expl, UID,
               Num, [Index|IXS], [Req|Reqs], Sts) :-
    get_dict(key, Index, "FRET"),
    (parse_fret_or_error(ProjName, CompName, LangEnv,
                         ReqName, Expl, UID, Num,
                         Index, Req)
    -> Cnt = 0
    ; Cnt = 1
    ),
    succ(Num, NextNum),
    make_fret_reqs(ProjName, CompName, LangEnv, ReqName, Expl, UID,
                   NextNum, IXS, Reqs, NextSts),
    Sts is Cnt + NextSts.
make_fret_reqs(ProjName, CompName, LangEnv, ReqName, Expl, UID,
               Num, [_|IXS], Reqs, Sts) :-
    % this index is not a FRET: skip it
    make_fret_reqs(ProjName, CompName, LangEnv, ReqName, Expl, UID,
                   Num, IXS, Reqs, Sts).


parse_fret_or_error(ProjName, CompName, LangEnv, ReqName, Expl, UID, Num, Index, Req) :-
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
    !,
    parse_fret_into_req(Context, LangEnv, LandoReqBase, English, Req).
parse_fret_into_req(Context, LangEnv, LandoReqBase, English, Req) :-
    parse_fret(Context, LangEnv, English, FretMent),
    !,
    put_dict(_{fret_req: FretMent}, LandoReqBase, LandoReq),
    Req = _{lando_req:LandoReq}.
parse_fret_into_req(Context, _, English, noreq) :-
    print_message(error, bad_frettish(Context, English)).

prolog:message(bad_frettish(Context, English)) -->
    [ 'BAD Frettish statement for ~w: ~w~n'
      - [ Context, English ] ].
