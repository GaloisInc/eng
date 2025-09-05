:- module(fret_kind2, [ fret_kind2/5,
                        normalize_kind2_var/2,
                        kind2_validate/7,
                        output_kind2_result/3,
                        match_req/2,
                        %% -- for testing only
                        connected_components/4
                      ]).

:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(strings)).
:- use_module(src/datafmts/lando).
:- use_module(src/lando_fret).
:- use_module(src/datafmts/frettish).
:- use_module(src/datafmts/ltl).
:- use_module(src/datafmts/lustre).
:- use_module(englib).
:- use_module(exprlang).
:- use_module(commands/exec_subcmds).
:- use_module(lando_fret).


%% ----------------------------------------------------------------------

% Generates a list of Kind2 outputs extracted from analyzing the set of FRETish
% statements.
%
%  EnumVals: (input) a list of enumeration definitions as (VarName, [VarValues])
%
%  FretReqs: list of dict:{added_vars:[..], lando_req:{lando_requirement:{fret_req:fretment(..)}}},
%
%  Vars: FRET JSON vars
%
%  Kind2Comps: output list of dicts for each connected component (compNum,
%  compName, kind2, files).  The kind2 field is the output kind2 contract and
%  node body, and files are all the implementation files referenced by the node
%  body.
%
fret_kind2(SSL, EnumVals, FretReqs, Vars, Kind2Comps) :-
    define_ltl_language,
    define_lustre_language,
    connected_components(FretReqs, Vars, 0, CComps),
    fret_to_kind2(SSL, EnumVals, Vars, CComps, Kind2Comps),
    %warn_about_skipped_models(Kind2Comps),
    true.

fret_to_kind2(_, _, _, [], []).
fret_to_kind2(SSL, EnumVals, Vars, [comp(N, CompName, Reqs, CVars)|CCs], [K2|K2s]) :-
    fret_to_kind2(SSL, EnumVals, Vars, CCs, K2s),
    format(atom(CName), '~w_~w', [ CompName, N ]),
    reqs_to_kind2(SSL, EnumVals, Vars, CName, Reqs, CVars, Kind2, NodeFiles),
    reqs_to_ids(Reqs, CompReqIds),
    K2 = _{ compNum: N,
            compName: CompName,
            compReqIds: CompReqIds,
            kind2: Kind2,
            files: NodeFiles
          }.

%% ----------------------------------------------------------------------

warn_about_skipped_models(K2S) :- warn_about_skipped_models(K2S, []).

warn_about_skipped_models([K2|K2S], NodeFiles) :-
    get_dict(files, K2, KNF),
    append(KNF, NodeFiles, NNF),
    warn_about_skipped_models(K2S, NNF).
warn_about_skipped_models([], NodeFiles) :-
    !,  % warn_skipped will backtrack, so make a backtracking fence here
    warn_skipped(NodeFiles).

warn_skipped(NodeFiles) :-
    eng:eng(system, model, kind2, NN, file, NF),
    \+ member(NF, NodeFiles),
    print_message(warning, skipped_model(NN, NF)),
    fail.  % try next non-member
warn_skipped(_).  % after all failures

prolog:message(skipped_model(ModelName, ModelFile)) -->
    [ 'No kind2 component for model ~w in ~w' - [ ModelName, ModelFile ]].

%% ----------------------------------------------------------------------

%% Group input requirements into "connected components".  The kind2 analysis is
%% done with past-time LTL, so requirements are connected if they share output
%% variable references in the responses.  This will return a list of the CC's
%% (Connected Components) that can be created from the provided requirements.

connected_components([], _, _, []).
connected_components([R|Reqs], Vars, N, [CC|CComps]) :-
    conncomp(Vars, N, R, Reqs, CC, RemReqs),
    succ(N, M),
    connected_components(RemReqs, Vars, M, CComps).

conncomp(Vars, N, Req, AllReqs, comp(N, CName, ConnReqs, OutVars), RemReqs) :-
    find_conn_reqs(Vars, [Req], AllReqs, ConnReqs, RemReqs),
    collect_out_vars(Vars, ConnReqs, OutVars),
    req_comp_name(Req, CName).

find_conn_reqs(Vars, Reqs, AllReqs, ConnReqs, RemReqs) :-
    member(R, AllReqs),
    member(I, Reqs),
    \+ member(R, Reqs),
    conn_reqs(Vars, I, R),
    !,
    find_conn_reqs(Vars, [R|Reqs], AllReqs, ConnReqs, RemReqs).
find_conn_reqs(_, Reqs, AllReqs, Reqs, RemReqs) :-
    rem_reqs(Reqs, AllReqs, RemReqs).

rem_reqs(_, [], []).
rem_reqs(ExclReqs, [R|RS], RemReqs) :-
    member(R, ExclReqs),
    !,
    rem_reqs(ExclReqs, RS, RemReqs).
rem_reqs(ExclReqs, [R|RS], [R|RemReqs]) :-
    rem_reqs(ExclReqs, RS, RemReqs).

conn_reqs(Vars, R1, R2) :-
    req_comp_name(R1, CName),
    req_comp_name(R2, CName), % verify reqs are for the same component
    req_out_varnames(Vars, R1, ROutVNames1),
    req_out_varnames(Vars, R2, ROutVNames2),
    member(V, ROutVNames2),
    member(V, ROutVNames1), % reqs share an output var
    !.  % no need to look further

collect_out_vars(_, [], []).
collect_out_vars(Vars, [R|RS], O) :-
    collect_out_vars(Vars, RS, S),
    req_out_varnames(Vars, R, VS),
    findall(V, (member(V, VS), \+ member(V, S)), H),
    append(H, S, O).

req_comp_name(Req, CompName) :-
    get_dict(lando_req, Req, LR),
    get_dict(fret_req, LR, Fretment),
    Fretment = fretment(_, _, component_info(Comp), _, _),
    get_dict(component, Comp, CompName).

req_out_varnames(Vars, FretReq, VS) :-
    get_dict(lando_req, FretReq, LR),
    get_dict(fret_req, LR, FR),
    % Could get just response fretment_vars, but this will filter on usage so
    % checking all variables reduces assumptions.
    findall(RVS, fretment_vars(_, FR, RVS), AllRVars),
    append(AllRVars, RVars),
    out_varnames(Vars, RVars, VS).

out_varnames(_, [], []).
out_varnames(Vars, [V|VS], [VName|VNMS]) :-
    var_name(V, VName),
    out_varname(Vars, VName),
    !,
    out_varnames(Vars, VS, VNMS).
out_varnames(Vars, [_|VS], VNMS) :- out_varnames(Vars, VS, VNMS).

out_varname(VS, VName) :-
    member(V, VS),
    \+ constr(_, _, _, _) = V,  % constructors cannot be vars
    get_dict(varname, V, VName),
    get_dict(usage, V, "Output").

reqs_to_ids([R|Reqs], [I|CompReqIds]) :-
    reqs_to_ids(Reqs, CompReqIds),
    get_dict(lando_req, R, LR),
    get_dict(req_name, LR, I).
reqs_to_ids([], []).

%% ----------------------------------------------------------------------

:- dynamic kind2_disallow_enums/0.

globals(EnumVals, Vars, Kind2Globals) :-
    enum_types(EnumVals, EGlobals),
    globalvals(EnumVals, Vars, VGlobals),
    append([EGlobals, VGlobals], Kind2Globals).


% Generate a kind2/lustre enumerated type for each enumerated variable.
enum_types(EnumsVals, Kind2Globals) :-
    enum_types_(EnumsVals, Kind2Globals).

enum_types_([], []).
enum_types_([(VN, _EV)|EVS], [TypeDef|TypeDefs]) :-
    kind2_disallow_enums, !,
    scenarios_type_name(VN, TypeName),
    format_str(TypeDef, 'type ~w = int;', [ TypeName ]),
    enum_types_(EVS, TypeDefs).
enum_types_([(VN, EV)|EVS], [TypeDef|TypeDefs]) :-
    enum_names(EV, EVNames),
    intercalate(EVNames, ", ", EVNMS),
    scenarios_type_name(VN, TypeName),
    format_str(TypeDef, 'type ~w = enum { ~w };', [ TypeName, EVNMS ]),
    enum_types_(EVS, TypeDefs).


enum_names([], []).
enum_names([(N,_)|ENS], [N|NS]) :- enum_names(ENS, NS).  % assumes numeric order.. but is that really important?

is_enum_val([(_,EN)|_], VName) :- enum_names(EN, ENames),
                                  member(VName, ENames),
                                  !.
is_enum_val([_|ENS], VName) :- is_enum_val(ENS, VName), !.

%% --------------------

globalvals(EnumVals, VS, [const|DS]) :-
    kind2_disallow_enums,  % Only active when this is set
    implicit_vars_val(EnumVals, VS, VDS),
    list_to_set(VDS, ADS),
    maplist([(N,V),O]>>format_str(O, '  ~w : int = ~w;', [N,V]), ADS, DS).

globalvals(_, [], []).
globalvals(EnumVals, [_|VS], DS) :- implicit_vars(EnumVals, VS, DS).

%% ----------------------------------------------------------------------

% Generate a set of mode specifications for each enumerated variable to enable
% kind2 defensive reachability and non-vacuity checking.
enum_modes(_, [], []).
enum_modes(Vars, [(N,VS)|EnumsVals], [[""|Modes]|ModeSpecs]) :-
    member(N, Vars),
    !,
    enum_mode(N, VS, 0, 99, Modes),
    enum_modes(Vars, EnumsVals, ModeSpecs).
enum_modes(Vars, [(IN,VS)|EnumsVals], [[""|Modes]|ModeSpecs]) :-
    scenarios_final_var_name(IN, N),
    member(N, Vars),
    !,
    enum_mode(N, VS, Modes),
    enum_modes(Vars, EnumsVals, ModeSpecs).
enum_modes(Vars, [_|EnumsVals], ModeSpecs) :-
    enum_modes(Vars, EnumsVals, ModeSpecs).

enum_mode(_, [], []).
enum_mode(Var, [(N,_)|VS], [Mode|Modes]) :-
    format(atom(Mode), '  mode ~w_~w ( require ~w = ~w; );', [Var, N, Var, N]),
    enum_mode(Var, VS, Modes).


%% ----------------------------------------------------------------------
% Generate various constraints on values

var_constraints(InpVars, OutVars, Enums, Constraints) :-
    kind2_disallow_enums, !,
    var_constrnts(InpVars, OutVars, Enums, Constraints).
var_constraints(_, _, _, []).

var_constrnts(_, _, [], []).
var_constrnts(InpVars, OutVars, [(IN,VS)|EnumsVals], [ICns,OCns|Other]) :-
    scenarios_final_var_name(IN, N),
    member(N, OutVars),
    !,
    get_min_max(VS, VSMin, VSMax),
    inp_constraints(VSMin, VSMax, IN, ICns),
    out_constraints(VSMin, VSMax,  N, OCns),
    var_constrnts(InpVars, OutVars, EnumsVals, Other).
var_constrnts(InpVars, OutVars, [(N,VS)|EnumsVals], [Constraint|Other]) :-
    member(N, InpVars),
    !,
    get_min_max(VS, VSMin, VSMax),
    inp_constraints(VSMin, VSMax, N, Constraint),
    var_constrnts(InpVars, OutVars, EnumsVals, Other).
var_constrnts(InpVars, OutVars, [_|EnumsVals], Constraints) :-
    var_constrnts(InpVars, OutVars, EnumsVals, Constraints).
var_constrnts(InpVars, OutVars, [_|EnumsVals], Constraints) :-
    var_constrnts(InpVars, OutVars, EnumsVals, Constraints).

get_min_max([], 0, 0).
get_min_max([(_,V)|VS], MinV, MaxV) :- get_min_max(VS, A, B),
                                       get_min_max(A, B, V, MinV, MaxV).
get_min_max(A, B, V, V, B) :- V < A, !.
get_min_max(A, B, V, A, V) :- B < V, !.
get_min_max(A, B, _, A, B).

out_constraints(Min, Max, Var, M) :-
    format(atom(M),
           'guarantee "~w output values" ~w <= ~w and ~w <= ~w;',
           [Var, Min, Var, Var, Max]).
inp_constraints(Min, Max, Var, M) :-
    format(atom(M),
           'assume "~w input values" ~w <= ~w and ~w <= ~w;',
           [Var, Min, Var, Var, Max]).


%% ----------------------------------------------------------------------

arg_decl(V, Decl) :-
    get_dict(varname, V, VarName),
    get_dict(type, V, VarType),
    convert_type(VarType, Type),
    format(atom(Decl), "~w : ~w", [ VarName, Type ]).


% All Internal and Mode variables should always be defined.  They may not be used
% by the requirements in this CC, but they shouldn't hurt anything by being
% defined.

implicit_vars(_, [], []).
implicit_vars(EnumVals, [_|VS], DS) :- implicit_vars(EnumVals, VS, DS).

implicit_vars_val(_, [], []).
%% implicit_vars_val(EnumVals, [constr(N,V,_,_)|VS], [(N,V)|DS]) :-
%%
%%   n.b. this is an alternative to the implicit_vars_val based in interating
%%   through the non-constrs below.  The reason this is not used is that the list
%%   only has constr entries for *used* values, but the mode specifications
%%   need *all* possible values.
%%
%%     !,
%%     implicit_vars_val(EnumVals, VS, DS).
implicit_vars_val(EnumVals, [constr(_,_,_,_)|VS], DS) :-
    !,
    % constr are skipped
    implicit_vars_val(EnumVals, VS, DS).
implicit_vars_val(EnumVals, [V|VS], DS) :-
    kind2_disallow_enums,  % Only active when this is set
    get_dict(type, V, TypeName),
    scenarios_type_name(VN, TypeName),
    atom_string(VN, VName),
    member((VName, VDecls), EnumVals),
    !,
    implicit_vars_val(EnumVals, VS, SubDS),
    append([VDecls, SubDS], DS).
implicit_vars_val(EnumVals, [_|VS], DS) :- implicit_vars_val(EnumVals, VS, DS).

input_vars(Reqs, CVars, Vars, Decls, Refs) :-
    input_vars_(Reqs, CVars, Vars, RRefs, RDecls),
    reverse(RRefs, Refs),
    reverse(RDecls, Decls).

input_vars_(_, _, [], _, []).
input_vars_(Reqs, CVars, [constr(_, _, _, _)|VS], Seen, Out) :-
    !,
    % constructors cannot be vars, skip this
    input_vars_(Reqs, CVars, VS, Seen, Out).
input_vars_(Reqs, CVars, [V|VS], [Name|SeenNames], Out) :-
    is_input_var(Reqs, CVars, V),
    !,
    arg_decl(V, Decl),
    input_vars_(Reqs, CVars, VS, SeenNames, DS),
    get_dict(varname, V, Name),
    add_if_not_present(Name, Decl, SeenNames, DS, Out).
input_vars_(Reqs, CVars, [_|VS], Seen, Out) :-
    input_vars_(Reqs, CVars, VS, Seen, Out).

is_input_var(Reqs, _, V) :-
    get_dict(usage, V, "Input"),  % filters out Internal, Mode, and Output vars
    get_dict(varname, V, Name),
    is_req_var(Name, Reqs),  % var is used directly by contract
    !.
is_input_var(_, CVars, V) :-
    % If a scenario output variable is present, the input variable should be made
    % available; this may not be needed for the contract, but a model will surely
    % need to base the output on the previous value (input) as well as any other
    % affecting inputs.
    get_dict(usage, V, "Input"),  % filters out Internal, Mode, and Output vars
    get_dict(varname, V, Name),
    scenarios_final_var_name(Name, OutName),
    member(OutName, CVars). % output var is used by contract, pass input as well


output_vars(Vars, OutNames, Decls, Names) :-
    output_vars_(Vars, OutNames, Names, Decls).
output_vars_(_, [], _, []).
output_vars_(Vars, [VN|VNS], [Name|SeenNames], Out) :-
    find_named_var(VN, Vars, V),
    get_dict(usage, V, "Output"),
    !,
    get_dict(varname, V, Name),
    arg_decl(V, Decl),
    output_vars_(Vars, VNS, SeenNames, DS),
    add_if_not_present(Name, Decl, SeenNames, DS, Out).
output_vars_(Vars, [_|VNS], Seen, Out) :-
    output_vars_(Vars, VNS, Seen, Out).

find_named_var(Name, VS, V) :-
    member(V, VS),
    \+ V = constr(_, _, _, _),  % constructors cannot be vars
    get_dict(varname, V, Name).

add_if_not_present(_, Decl, [], DS, [Decl|DS]).   % end-of list: add Decl
add_if_not_present(Var, _, [Var|_], DS, DS) :- !. % Var is in list, no add
add_if_not_present(Var, Decl, [_|VS], DS, Out) :- % No match, check next
    add_if_not_present(Var, Decl, VS, DS, Out).

convert_type("integer", "int") :- !.
convert_type("boolean", "bool") :- !.
convert_type(integer, "int") :- !.
convert_type(boolean, "bool") :- !.
convert_type(_, "int") :- kind2_disallow_enums, !.
convert_type(Other, Other).

req_internalvars([], [], [], []).
req_internalvars([R|RS], [G|GS], [D|DS], Helpers) :-
    get_dict(lando_req, R, LR),
    get_dict(req_name, LR, RID),
    normalize_kind2_var(RID, V),
    !,

    get_dict(fret_req, LR, Fretish),
    emit_fretish(Fretish, FT),

    fretish_ptltl(Fretish, cocospec(E)),
    collect_kind2_helpers(E, MyHelpers),
    emit_CoCoSpec(E, CoCo),

    format(atom(D), '(* Req: ~w *)~n~n  var ~w : bool = ~w;~n~n', [ FT, V, CoCo ]),
    format(atom(G), 'guarantee "~w" ~w;', [RID, V]),
    req_internalvars(RS, GS, DS, MoreHelpers),
    append(MyHelpers, MoreHelpers, Helpers).

is_req_var(VName, Reqs) :-
    member(FretReq, Reqs),
    get_dict(lando_req, FretReq, LR),
    get_dict(fret_req, LR, FR),
    findall(RVS, fretment_vars(_, FR, RVS), AllRVars),
    append(AllRVars, RVars),
    maplist(var_name, RVars, RVNames),
    member(VName, RVNames).

normalize_kind2_var(Inp, Out) :-
    subst('-', '_', Inp, Out).

match_req(Spec1, Spec1) :- !.
match_req(Spec1, Spec2) :- atom_string(Spec2, Spec1), !.
match_req(Spec1, Spec2) :- atom_string(Spec1, Spec2), !.
match_req(Spec1, Spec2) :- normalize_kind2_var(Spec1, Spec2), !.
match_req(Spec1, Spec2) :- normalize_kind2_var(Spec2, Spec1), !.
match_req(Spec1, Spec2) :- normalize_kind2_var(Spec1, Spec1N), atom_string(Spec1N, Spec2), !.
match_req(Spec1, Spec2) :- normalize_kind2_var(Spec1, Spec1N), atom_string(Spec2, Spec1N), !.
match_req(Spec1, Spec2) :- normalize_kind2_var(Spec2, Spec2N), atom_string(Spec2N, Spec1), !.
match_req(Spec1, Spec2) :- normalize_kind2_var(Spec2, Spec2N), atom_string(Spec1, Spec2N), !.
match_req(Spec1, Spec2) :- atom_string(Spec2A, Spec2), normalize_kind2_var(Spec1, Spec2A), !.


%% ----------------------------------------------------------------------

%% Perform kind2 validation of the specified file.
%
% Sts is: ignored, invalid, passed, failed, failed_as_expected,
%         unexpectedly_passed
%
%     * invalid = kind2 run failed (input syntax) or output processing failed
%                 (e.g. output file not generated)
%
kind2_validate(Context, Kind2File, OutDirectory, contract, LustreFile, ResultFile, Status) :-
    validate(Context, Kind2File, OutDirectory, "--enable CONTRACTCK", LustreFile, ResultFile, Status).
kind2_validate(Context, Kind2File, OutDirectory, model, LustreFile, ResultFile, Status) :-
    validate(Context, Kind2File, OutDirectory, "", LustreFile, ResultFile, Status).
validate(Context, Kind2File, OutDirectory, Args, LustreFile, ResultFile, Status) :-
    do_exec(Context, "kind2 lando fret validation",
            [ 'InpFile' = Kind2File,
              'OutDir' = OutDirectory,
              'Kind2Args' = Args,
              'JSONFile' = ResultFile ],
            [ "kind2 -json {Kind2Args} --output_dir {OutDir} --timeout 60 {InpFile} > {JSONFile}"
              % --lus_strict
            ], [], ".", Sts),
    ( process_kind2_results(Sts, LustreFile, ResultFile, Status), !
    ; Status = [invalid]
    ).


process_kind2_results(30, _, _, [invalid]) :-
    !,
    print_message(error, kind2_timeout).
process_kind2_results(_, LustreFile, ResultFile, Status) :-
    open(ResultFile, read, ResultStrm),
    json_read_dict(ResultStrm, Results),
    close(ResultStrm),
    !,  % no backtracking
    show_kind2_results(LustreFile, Results, Status).

prolog:message(kind2_timeout) --> [ 'Timeout running kind2 analysis' ].

show_kind2_results(_, [], []).
show_kind2_results(LustreFile, [O|OS], [Sts|Status]) :-
    get_dict(objectType, O, OType),
    show_kind2_results(LustreFile, O, OType, Sts),
    show_kind2_results(LustreFile, OS, Status).

show_kind2_results(_, _, "kind2Options", ignored) :- !. % ignored
show_kind2_results(_, _, "analysisStart", ignored) :- !. % ignored
show_kind2_results(_, _, "analysisStop", ignored) :- !. % ignored
show_kind2_results(_, O, "log", Status) :-
    !,
    get_dict(level, O, Level),
    show_kind2_log(O, Level, Status).
show_kind2_results(LustreFile, O, "realizabilityCheck", Status) :-
    !,
    get_dict(result, O, Result),
    show_kind2_result(LustreFile, O, Result, Status).
show_kind2_results(_, O, "satisfiabilityCheck", Status) :-
    !,
    get_dict(result, O, Result),
    show_kind2_satresult(O, Result, Status).
show_kind2_results(LustreFile, O, "property", Status) :-
    !,
    get_dict(answer, O, Answer),
    get_dict(value, Answer, PropSts),
    show_kind2_result(LustreFile, O, PropSts, Status).
show_kind2_results(_, _, OType, failed) :-
    print_message(warning, unrecognized_kind2_result_type(OType)).


show_kind2_log(O, "error", failed) :-
    !,
    get_dict(source, O, Source),
    get_dict(file, O, File),
    get_dict(line, O, Line),
    get_dict(column, O, Col),
    get_dict(value, O, Msg),
    print_message(error, kind2_log_error(Source, File, Line, Col, Msg)).
show_kind2_log(_, _, ignored).  % All other log levels ignored


show_kind2_result(LustreFile, O, "realizable", passed) :-
    !,
    get_dict(runtime, O, RT),
    get_dict(value, RT, Time),
    get_dict(unit, RT, Unit),
    realizable(LustreFile, Time, Unit).
show_kind2_result(LustreFile, O, "unrealizable", failed) :-
    !,
    get_dict(conflictingSet, O, Conflicts),
    get_dict(size, Conflicts, CSize),
    get_dict(nodes, Conflicts, [Node|Nodes]),
    (Nodes == [] ; print_message(warning, other_unrealizable_nodes(Nodes))),
    get_dict(elements, Node, Elems),
    maplist(get_dict(name), Elems, Names),
    maplist(normalize_kind2_var, Names, CNames),
    % ContractNames are direct variable names if this is a "node imported"
    % contract specification.  If it is a "contract" specification, a ghost
    % variable is created in the primary node for each contract guarantee and it
    % is referenced in the ContractNames as
    % "contractname[UNIQUENODEID].ContractVar"; here, we extract just the
    % ContractVar from either form.
    maplist(simple_varname, CNames, ContractNames),
    get_dict(deadlockingTrace, O, [Trace|Traces]),
    (Traces == [] ; print_message(warning, other_unrealizable_traces(Traces))),
    get_dict(streams, Trace, Streams),
    findall(N, (member(Stream, Streams), trace_input(Stream, N)), Inputs),
    findall(N, (member(Stream, Streams), trace_output(Stream, N)), Outputs),
    append(Inputs, Outputs, InsOuts),
    append(InsOuts, ContractNames, InterestingVars),
    !,
    show_stream_steps(InterestingVars, Streams),
    get_dict(runtime, O, RT),
    get_dict(value, RT, Time),
    get_dict(unit, RT, Unit),
    unrealizable(LustreFile, Time, Unit, CSize, Names, ContractNames).
show_kind2_result(_, O, "falsifiable", failed_as_expected) :-
    get_dict(name, O, FullName),
    simple_varname(FullName, Name),
    allowed_kind2_failure(Name),
    !,
    falsifiable_result(O, warning, Name, kind2_falsifiable_expected(Name)).
show_kind2_result(_, O, "falsifiable", failed) :-
    get_dict(name, O, FullName),
    simple_varname(FullName, Name),
    !,
    falsifiable_result(O, error, Name, kind2_falsifiable(Name)).
show_kind2_result(_, O, "reachable", R) :-
    !,
    timed_result(O, reachable, R).
show_kind2_result(_, O, "unreachable", failed_as_expected) :-
    get_dict(name, O, FullName),
    simple_varname(FullName, Name),
    allowed_kind2_failure(Name),
    !,
    call(output_kind2_result, Name, warning, kind2_unreachable_expected(Name)).
show_kind2_result(_, O, "unreachable", failed) :-
    !,
    get_dict(name, O, FullName),
    simple_varname(FullName, Name),
    call(output_kind2_result, Name, error, kind2_unreachable(Name)).
show_kind2_result(_, O, "valid", passed) :-
    !,
    get_dict(runtime, O, RT),
    get_dict(value, RT, Time),
    get_dict(unit, RT, Unit),
    get_dict(name, O, FullName),
    simple_varname(FullName, Name),
    call(output_kind2_result, Name, success, kind2_valid(Time, Unit, Name)).
show_kind2_result(_, _, R, failed) :-
    print_message(error, unknown_kind2_result(R)).

:- dynamic contract/2.

realizable(_, 0.0, _) :-
    % Suppress messages for instantly realizable elements: these are generally
    % the ancillary nodes and functions supporting the primary.  Another method
    % for detecting these would be to check the surrounding
    % analysisStart/analysisStop, except the realizable realizablityCheck outputs
    % are within those brackets and the unrealizable realizablityCheck is outside
    % of those brackets... why?
    !.
realizable(LustreFile, Time, Unit) :-
    contract(LustreFile, ReqIDs),
    !,
    realizable_id(Time, Unit, ReqIDs).
realizable(X, Time, Unit) :-
    format('no contract specification for ~w~n', [X]),
    print_message(success, kind2_realizable(Time, Unit)).

realizable_id(_, _, []).
realizable_id(Time, Unit, [R|RS]) :-
    call(output_kind2_result, R, success, kind2_realizable(R, Time, Unit)),
    realizable_id(Time, Unit, RS).

unrealizable(LustreFile, Time, Unit, _, _, Conflicts) :-
    % First, get all contracts in this file that were NOT in conflict and report them as realizable
    contract(LustreFile, ReqIDs),
    member(ReqID, ReqIDs),
    \+ find_reqid(ReqID, Conflicts),
    call(output_kind2_result, ReqID, success, kind2_realizable(ReqID, Time, Unit)),
    % Now backtrack to member above for the next ReqID. When ReqIDs depleted,
    % fall through to next below to report on the unrealizable requirements.
    fail.
unrealizable(_, _, _, CSize, Names, Conflicts) :-
    unrealizable_(CSize, Names, Conflicts).
unrealizable_(CSize, Names, [C|CS]) :-
    call(output_kind2_result, C, error, kind2_unrealizable(CSize, Names)),
    unrealizable_(CSize, Names, CS).
unrealizable_(_, _, []).

find_reqid(ReqID, [C|_]) :- match_req(ReqID, C), !.
find_reqid(ReqID, [_|CS]) :- find_reqid(ReqID, CS).

timed_result(O, reachable, unexpectedly_passed) :-
    get_dict(name, O, FullName),
    simple_varname(FullName, Name),
    allowed_kind2_failure(Name),
    !,
    get_dict(runtime, O, RT),
    get_dict(value, RT, Time),
    get_dict(unit, RT, Unit),
    call(output_kind2_result, Name, success, kind2_reachable_unexpectedly(Time, Unit, Name)).
timed_result(O, reachable, passed) :-
    get_dict(name, O, FullName),
    simple_varname(FullName, Name),
    get_dict(runtime, O, RT),
    get_dict(value, RT, Time),
    get_dict(unit, RT, Unit),
    call(output_kind2_result, Name, success, kind2_reachable(Time, Unit, Name)).

falsifiable_result(O, MsgMode, Name, ErrMsg) :-
    get_dict(counterExample, O, [CounterEx|OtherCounterEx]),
    get_dict(streams, CounterEx, Streams),
    findall(N, (member(Stream, Streams), trace_input(Stream, N)), Inputs),
    findall(N, (member(Stream, Streams), trace_output(Stream, N)), Outputs),
    append(Inputs, Outputs, InterestingVars),
    !,
    show_stream_steps(InterestingVars, Streams),
    call(output_kind2_result, Name, MsgMode, ErrMsg).

:- dynamic output_kind2_result/3.

output_kind2_result(_ReqName, Level, Msg) :- print_message(Level, Msg).


allowed_kind2_failure(Name) :-
    eng:eng(system, model, kind2, allowed_failure, AF),
    split_string(AF, ",", " ", AFNames),
    member(Name, AFNames).

simple_varname(ContractRef, ContractVar) :-
    string_concat(_, Right, ContractRef),
    string_concat("].", Mid, Right),
    trim_trailing_index(Mid, ContractVar),
    !.
simple_varname(CV, CV).

trim_trailing_index(V, Simple) :-
    string_concat(Simple, Right, V),
    string_concat("[", _, Right),
    !.
trim_trailing_index(V, V).

show_kind2_satresult(O, "satisfiable", satisfiable) :-
    get_dict(runtime, O, RT),
    get_dict(value, RT, 0.0),  % just environment, not useful
    !.
show_kind2_satresult(O, "satisfiable", satisfiable) :-
    !,
    get_dict(runtime, O, RT),
    get_dict(value, RT, Time),
    get_dict(unit, RT, Unit),
    print_message(success, kind2_satisfiable(Time, Unit)). % which CC?
show_kind2_satresult(_, R, unsure) :-
    print_message(error, unknown_kind2_satresult(R)).


trace_input(StreamEntry, Name) :-
    get_dict(class, StreamEntry, "input"),
    get_dict(name, StreamEntry, Name).

trace_output(StreamEntry, Name) :-
    get_dict(class, StreamEntry, "output"),
    get_dict(name, StreamEntry, Name).

show_stream_steps(Vars, Streams) :-
    maplist(string_length, Vars, ColSizes),
    show_stream_steps(Vars, Streams, ColSizes, 0, Fmt, Lines),
    format_lines(Fmt, [["Step"|Vars]|Lines]).
show_stream_steps(Vars, Streams, ColSizes, StepNum, Fmt, [[StepNum|Vals]|Lines]) :-
    maplist(get_step_val(Streams, StepNum), Vars, Vals, ValLens),
    !,
    maplist(string_max_length, ColSizes, ValLens, UpdColSizes),
    succ(StepNum, NextStepNum),
    show_stream_steps(Vars, Streams, UpdColSizes, NextStepNum, Fmt, Lines).
show_stream_steps(_, _, ColSizes, _, Fmt, []) :-
    maplist(succ, ColSizes, SZS), % +1 on each for the space separator
    make_format([4|SZS], Fmt).


string_max_length(CurLen, MaxLen, MaxLen) :- CurLen < MaxLen.
string_max_length(MaxLen, _, MaxLen).

make_format([S|SZ], Fmt) :-
    make_format(SZ, SubFmt),
    format(atom(Fmt), '~w~d~w ~w', ['~w~t~', S, '+', SubFmt]).
make_format([], "~n").

get_step_val([S|_], StepNum, Var, Val, ValLen) :-
    get_dict(name, S, Var),
    get_dict(instantValues, S, Vals),
    get_valnum(Vals, StepNum, RawVal),
    format_colval(RawVal, Val),
    string_length(Val, ValLen).
get_step_val([_|Streams], StepNum, Var, Val, ValLen) :-
    get_step_val(Streams, StepNum, Var, Val, ValLen).

format_colval(RawVal, Val) :-
    format(atom(V), '~w', [RawVal]),
    atom_string(V, Val).

get_valnum(StepVals, StepNum, Val) :- member([StepNum,Val], StepVals).

prolog:message(kind2_realizable(Time, Unit)) -->
    [ 'Realizable    (~w ~w)' - [ Time, Unit ] ].
prolog:message(kind2_realizable(ReqID, Time, Unit)) -->
    [ 'Realizable    (~w ~w): ~w' - [ Time, Unit, ReqID ] ].
prolog:message(kind2_reachable(Time, Unit, Name)) -->
    [ 'Reachable     (~w ~w): ~w' - [ Time, Unit, Name ] ].
prolog:message(kind2_reachable_unexpectedly(Time, Unit, Name)) -->
    [ 'Reachable     (~w ~w): ~w  ... EXPECTED THIS TO FAIL!'
      - [ Time, Unit, Name ] ].
prolog:message(kind2_valid(Time, Unit, Name)) -->
    [ 'Valid         (~w ~w): ~w' - [ Time, Unit, Name ] ].
prolog:message(kind2_satisfiable(Time, Unit)) -->
    [ 'Satisfiable   (~w ~w)' - [ Time, Unit ] ].
prolog:message(kind2_unrealizable(Num, Names)) -->
    [ 'UNREALIZABLE, ~w conflicts: ~w' - [ Num, Names ] ].
prolog:message(kind2_unreachable(Name)) -->
    [ 'UNREACHABLE: ~w' - [ Name ] ].
prolog:message(kind2_unreachable_expected(Name)) -->
    [ 'UNREACHABLE: ~w  (expected this failure)' - [ Name ] ].
prolog:message(kind2_falsifiable(Name)) -->
    [ 'FALSIFIABLE: ~w' - [ Name ] ].
prolog:message(kind2_falsifiable_expected(Name)) -->
    [ 'FALSIFIABLE: ~w  (expected this failure)' - [ Name ] ].
prolog:message(other_unrealizable_nodes(Nodes)) -->
    [ 'additional nodes not handled: ~w' - [ Nodes ] ].
prolog:message(other_unrealizable_traces(Traces)) -->
    [ 'additional traces not handled: ~w' - [ Traces ] ].
prolog:message(other_unrealizable_streams(Streams)) -->
    [ 'additional streams not handled: ~w' - [ Streams ] ].
prolog:message(unknown_kind2_result(R)) -->
    [ 'Unknown kind2 realizability result: ~w~n' - [ R ] ].
prolog:message(unknown_kind2_satresult(R)) -->
    [ 'Unknown kind2 satisfiability result: ~w~n' - [ R ] ].
prolog:message(unrecognized_kind2_result_type(OType)) -->
    [ 'Unrecognized result type: ~w~n' - [OType] ].
prolog:message(kind2_log_error(Source, File, Line, Col, Msg)) -->
    [ '~w:~w:~w: ~w error: ~w~n' - [ File, Line, Col, Source, Msg ] ].

%% ----------------------------------------------------------------------

is_member(X, Y) :- member(Y, X).

%% Called to generate an output file for kind2 contract analysis.
%
% EnumVals: list of values which are an enumeration
% Vars: all variable definitions
% CompName: the component name (used to name nodes and contracts)
% Reqs: all fret requirements
% CVars: the output variables for this contract
% Kind2: returns the Kind2 specification

reqs_to_kind2(SSL, EnumVals, Vars, CompName, Reqs, CVars, Kind2, FileNames) :-

    cc_model_impls(CVars, MdlInpVars, Calls, FileNames),
    intercalate(Calls, "\n", NodeCalls),
    !,

    globals(EnumVals, Vars, Kind2Globals),
    !,

    enum_modes(CVars, EnumVals, ModeSpecs),
    append(ModeSpecs, AllModes),
    intercalate(AllModes, "\n", Modes),

    intercalate(Kind2Globals, "\n", GlobalDecls),
    implicit_vars(EnumVals, Vars, Kind2Decls),
    !,
    intercalate(Kind2Decls, "\n  ", NodeDecls),

    input_vars(Reqs, CVars, Vars, Kind2Input, Kind2Args),
    exclude(is_member(Kind2Args), MdlInpVars, MoreArgsNeeded),
    maplist(collect_var(SSL), MoreArgsNeeded, MoreArgsCollected),
    maplist(arg_decl, MoreArgsCollected, MoreArgDecls),
    append([Kind2Input, MoreArgDecls], Kind2InputComplete),
    !,
    intercalate(Kind2Input, ";\n                    ", ContractArgDecls),
    intercalate(Kind2InputComplete, ";\n                    ", NodeArgDecls),
    intercalate(Kind2Args, ", ", ContractArgs),


    output_vars(Vars, CVars, Kind2OutputDecls, Kind2Outputs),
    !,
    intercalate(Kind2OutputDecls, ";\n                    ", NodeRet),
    intercalate(Kind2Outputs, ", ", ContractOutputs),
    req_internalvars(Reqs, Kind2Guarantees, Kind2ReqVars, HelpersNeeded),
    intercalate(Kind2ReqVars, "\n  ", NodeReqDecls),
    intercalate(Kind2Guarantees, "\n  ", NodeGuarantees),

    !,
    var_constraints(Kind2Args, CVars, EnumVals, ConstraintSpecs),
    intercalate(ConstraintSpecs, "\n  ", Constraints),

    !,

    [NodeName] = [ CompName ],
    !,
    list_to_set(HelpersNeeded, HList),
    maplist(kind2_helper_def, HList, HelperImpls),
    intercalate(HelperImpls, "\n", Helpers),

    Kind2 = {|string(NodeName,
                     NodeArgDecls,
                     ContractArgs, ContractArgDecls,
                     NodeRet, ContractOutputs,
                     NodeDecls,
                     NodeReqDecls,
                     NodeGuarantees,
                     GlobalDecls,
                     NodeCalls,
                     Constraints,
                     Modes,
                     Helpers)||
| {Helpers}
|
| {GlobalDecls}
|
| ----------------------------------------------------------------------
| contract {NodeName}Spec( {ContractArgDecls} )
| returns ( {NodeRet} );
| let
|   {NodeDecls}
|   {Constraints}
|   {NodeReqDecls}
|   {NodeGuarantees}
|   {Modes}
| tel
|
| ----------------------------------------------------------------------
| node {NodeName} ( {NodeArgDecls} )
| returns ( {NodeRet} );
|   (*@contract
|      import {NodeName}Spec({ContractArgs})
|             returns ({ContractOutputs});
|   *)
| let
|   --%MAIN;
|   {NodeCalls}
| tel
|}.

%% ----------------------------------------------------------------------

:- dynamic kind2_no_model/0.

cc_model_impls(_, [], [], []) :- kind2_no_model, !.
cc_model_impls(OutVars, InpVars, Calls, NodeImplFileNames) :-
    findall(I, cc_model_impl_name(OutVars, I), ImplNames),
    findall(V, cc_model_impl_inpvars(ImplNames, V), InpVarSets),
    append(InpVarSets, InpVarList),
    list_to_set(InpVarList, InpVars),
    findall(C, cc_model_impl_call(ImplNames, C), Calls),
    findall(N, cc_model_impl_file(ImplNames, N), NodeImplFileNames).

cc_model_impl_name(OutVars, ImplName) :-
    eng:eng(system, model, kind2, ImplName, output, IOutVars),
    % n.b. expect a single output variable, but allow multiples separated by
    % commas here.
    split_string(IOutVars, ",", " ", VS),
    findall(V, (member(V, VS), member(V, OutVars)), NVS),
    length(VS, VSLen),
    length(NVS, VSLen).  % lengths are the same

cc_model_impl_inpvars(ImplNames, InpVars) :-
    member(NodeName, ImplNames),
    eng:eng(system, model, kind2, NodeName, inputs, InpArgs),
    split_string(InpArgs, ",", " ", InpArgList),
    list_to_set(InpArgList, InpVars).

cc_model_impl_call(ImplNames, Call) :-
    member(NodeName, ImplNames),
    eng:eng(system, model, kind2, NodeName, output, OutArg),
    eng:eng(system, model, kind2, NodeName, inputs, InpArgs),
    format(atom(Call), '  ~w = ~w(~w);', [ OutArg, NodeName, InpArgs ]).

cc_model_impl_file(ImplNames, FName) :-
    member(NodeName, ImplNames),
    eng:eng(system, model, kind2, NodeName, file, FName).


%% ----------------------------------------------------------------------

collect_kind2_helpers(op(O, _), Helpers) :-
    O =.. [Op|Args],
    maplist(collect_kind2_helpers, Args, AHS),
    append(AHS, ArgHelpers),
    kind2_helper(Op, OH),
    append([OH, ArgHelpers], AllHelpers),
    list_to_set(AllHelpers, Helpers).
collect_kind2_helpers(_, []).

kind2_helper(ltlH, ["H"]).
kind2_helper(ltlH_bound, ["HT", "OT", "delay", "OTlore"]).
kind2_helper(ltlSI, ["SI"]).
kind2_helper(ltlS, ["S"]).
kind2_helper(ltlST, ["ST", "S", "OT", "delay", "OTlore"]).
kind2_helper(ltlSIT, ["SIT", "SI", "OT", "delay", "OTlore"]).
kind2_helper(ltlO, ["O"]).
kind2_helper(ltlO_bound, ["OT", "delay", "OTlore"]).
kind2_helper(preBool, ["preBool"]).
kind2_helper(preInt, ["preInt"]).
kind2_helper(preReal, ["preReal"]).
kind2_helper(absInt, ["absInt"]).
kind2_helper(absReal, ["absReal"]).
kind2_helper(maxInt, ["maxInt"]).
kind2_helper(maxReal, ["maxReal"]).
kind2_helper(minInt, ["minInt"]).
kind2_helper(minReal, ["minReal"]).
kind2_helper(delay, ["delay"]).
kind2_helper(ltlY, ["YtoPre"]).
kind2_helper(ltlZ, ["ZtoPre"]).
kind2_helper(_, []).

kind2_helper_def("H", Helper) :-
    !,
    Helper = {|string||
| --Historically: X has always been true (so-far).
| -- As soon as X is false once, Y will be false forever
| -- (falling edge)
| node H(X:bool) returns (Y:bool);
| let
|     Y = X -> (X and (pre Y));
| tel
|
|}.
kind2_helper_def("HT", Helper) :-
    !,
    Helper = {|string||
| -- Timed Historically: general case
| -- True if X has been true, and for R ticks afterwards, false thereafter
| -- Always true for at least R ticks of the timeline, even if X is never true
| -- L is ignored
| node HT( L: int;  R: int; X: bool) returns (Y: bool);
| let
|   Y = not OT(L, R, not X);
| tel
|
|}.
kind2_helper_def("O", Helper) :-
    !,
    Helper = {|string||
| --Once
| --  The first time X is true, Y is true forever.
| --  X has been true at least once.
| --  Rising edge.
| node O(X:bool) returns (Y:bool);
| let
|  Y = X or (false -> pre Y);
| tel
|
|}.
kind2_helper_def("OT", Helper) :-
    !,
    Helper = {|string||
| --Timed Once: general case
| -- True R ticks after each X first true until L ticks after X is last true
| node OT( L: int;  R: int; X: bool) returns (Y: bool);
| var  D:bool;
| let
|   D=delay(X, R);
|   Y=OTlore(L-R, D);
| tel
|
|}.
kind2_helper_def("OTlore", Helper) :-
    !,
    Helper = {|string||
| --Timed Once: less than or equal to N
| --  True every X and for N ticks afterward
| node OTlore( N: int; X: bool) returns (Y: bool);
|     var C:int;
| let
|     C = if X then 0
|         else (-1 -> pre C + (if pre C <0 then 0 else 1));
|
|     Y = 0 <= C and C <= N;
| tel
|
|}.
kind2_helper_def("S", Helper) :-
    !,
    Helper = {|string||
| -- Since
| -- Y since X
| -- X is true, then Y until false
| -- X has been true at some point in the past, and Y has been true since then
| node S(X,Y: bool) returns (Z:bool);
| let
| Z = X or (Y and (false -> pre Z));
| tel
|
|}.
kind2_helper_def("SI", Helper) :-
    !,
    Helper = {|string||
| --Y since inclusive X
| --  Y is enabler/reset: while Y, from X true onwards
| -- or
| --  at X, Y until false, then reset
| node SI(X,Y: bool) returns (Z:bool);
| let
| Z = Y and (X or (false -> pre Z));
| tel
|
|}.
kind2_helper_def("ST", Helper) :-
    !,
    Helper = {|string||
| -- Timed Since: general case
| -- R ticks after X is true, for L ticks if/while Y remains true
| node ST( L: int;  R: int; X: bool; Y: bool)  returns (Z: bool);
| let
|   Z = S(X, Y) and OT(L, R, X);
| tel
|
|}.
kind2_helper_def("SIT", Helper) :-
    !,
    Helper = {|string||
| -- Timed Since Inclusive: general case
| -- R ticks after X is true, for L ticks if X or while Y remains true
| node SIT( L: int;  R: int; X: bool; Y: bool) returns (Z: bool);
| let
|   Z = SI(X, Y) and OT(L, R, X);
| tel
|
|}.
kind2_helper_def("YtoPre", Helper) :-
    !,
    Helper = {|string||
| -- The equivalent of LTL Y in Lustre.
| --   Initially false, then the previous value of X (false, delay X by one)
| node YtoPre(X: bool) returns (Y:bool);
| let
|   Y = false -> pre X;
| tel
|
|}.
kind2_helper_def("ZtoPre", Helper) :-
    !,
    Helper = {|string||
| -- The equivalent of LTL Z in Lustre.
| --   Initially true, then the previous value of X (true, delay X by one)
| node ZtoPre(X: bool) returns (Y:bool);
| let
|   Y = true -> pre X;
| tel
|
|}.
kind2_helper_def("delay", Helper) :-
    !,
    Helper = {|string||
| node delay(X:bool;  R:int) returns(Y:bool);
| var X1, X2, X3, X4, X5, X6, X7, X8, X9, X10 : bool;
| let
|   Y = if (R=0) then X
|       else (if (R=1) then X1
|       else (if (R=2) then X2
|       else (if (R=3) then X3
|       else (if (R=4) then X4
|       else (if (R=5) then X5
|       else (if (R=6) then X6
|       else (if (R=7) then X7
|       else (if (R=8) then X8
|       else (if (R=9) then X9
|       else (if (R=10) then X10
|       else false (* n.b. only handles R <= 10 *)
|       ))))))))));
|   X1 = false -> pre X;
|   X2 = false -> pre X1;
|   X3 = false -> pre X2;
|   X4 = false -> pre X3;
|   X5 = false -> pre X4;
|   X6 = false -> pre X5;
|   X7 = false -> pre X6;
|   X8 = false -> pre X7;
|   X9 = false -> pre X8;
|   X10 = false -> pre X9;
| tel
|
|}.
kind2_helper_def("preInt", Helper) :-
    !,
    Helper = {|string||
| -- Pre for integers, with an initial value at FTP
| node preInt(InitialValue, X: int) returns (Y:int);
| let
|   Y = InitialValue -> pre X;
| tel
|
|}.
kind2_helper_def("preReal", Helper) :-
    !,
    Helper = {|string||
| -- Pre for reals, with an initial value at FTP
| node preReal(InitialValue, X: real) returns (Y:real);
| let
|   Y = InitialValue -> pre X;
| tel
|
|}.
kind2_helper_def("preBool", Helper) :-
    !,
    Helper = {|string||
| -- Pre for booleans, with an initial value at FTP
| node preBool(InitialValue, X: bool) returns (Y:bool);
| let
|   Y = InitialValue -> pre X;
| tel
|
|}.
kind2_helper_def("absInt", Helper) :-
    !,
    Helper = {|string||
| -- Absolute value for integers
| function absInt(x:int) returns(y: int);
| let
|   y = if (x >= 0) then x else -x;
| tel
|
|}.
kind2_helper_def("absReal", Helper) :-
    !,
    Helper = {|string||
| -- Absolute value for reals
| function absReal(x:real) returns(y: real);
| let
|   y = if (x >= 0.0) then x else -x;
| tel
|
|}.
kind2_helper_def("maxInt", Helper) :-
    !,
    Helper = {|string||
| -- Maximum value between two integers
| function maxInt (a : int; b : int)
| returns (z : int);
| let
|   z = (if (((a) >= (b))) then (a) else (b));
| tel
|
|}.
kind2_helper_def("maxReal", Helper) :-
    !,
    Helper = {|string||
| -- Maximum value between two reals
| function maxReal (a : real; b : real)
| returns (z : real);
| let
|   z = (if (((a) >= (b))) then (a) else (b));
| tel
|
|}.
kind2_helper_def("minInt", Helper) :-
    !,
    Helper = {|string||
| -- Minimum value between two integers
| function minInt (a : int; b : int)
| returns (z : int);
| let
|   z = (if (((a) <= (b))) then (a) else (b));
| tel
|
|}.
kind2_helper_def("minReal", Helper) :-
    !,
    Helper = {|string||
| -- Minimum value between two reals
| function minReal (a : real; b : real)
| returns (z : real);
| let
|   z = (if (((a) <= (b))) then (a) else (b));
| tel
|
|}.
kind2_helper_def(H, "") :-
    print_message(error, unknown_helper(H)),
    fail.

prolog:message(unknown_helper(H)) -->
    [ "Unknown Kind2/Lustre helper requested: ~w"-[H]].
