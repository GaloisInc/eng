:- module(fret_kind2, [ fret_kind2/4,
                        normalize_kind2_var/2,
                        kind2_validate/6,
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
:- use_module(englib).
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
fret_kind2(EnumVals, FretReqs, Vars, Kind2Comps) :-
    connected_components(FretReqs, Vars, 0, CComps),
    fret_to_kind2(EnumVals, Vars, CComps, Kind2Comps),
    warn_about_skipped_models(Kind2Comps).

fret_to_kind2(_, _, [], []).
fret_to_kind2(EnumVals, Vars, [comp(N, CompName, Reqs, CVars)|CCs], [K2|K2s]) :-
    fret_to_kind2(EnumVals, Vars, CCs, K2s),
    format(atom(CName), '~w_~w', [ CompName, N ]),
    reqs_to_kind2(EnumVals, Vars, CName, Reqs, CVars, Kind2, NodeFiles),
    K2 = _{ compNum: N,
            compName: CompName,
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
    get_dict(requirement, FretReq, Req),
    get_dict(semantics, Req, ReqSem),
    get_dict(variables, ReqSem, ReqVars),
    out_varnames(Vars, ReqVars, VS).

out_varnames(_, [], []).
out_varnames(Vars, [V|VS], [V|VNMS]) :-
    out_varname(Vars, V),
    !,
    out_varnames(Vars, VS, VNMS).
out_varnames(Vars, [_|VS], VNMS) :- out_varnames(Vars, VS, VNMS).

out_varname([V|_], VName) :- get_dict(variable_name, V, VName),
                             !,
                             get_dict(idType, V, "Output").
out_varname([_|VS], VName) :- out_varname(VS, VName).

%% ----------------------------------------------------------------------

% Generate a kind2/lustre enumerated type for each enumerated variable.
enum_types(EnumsVals, Kind2Globals, VarTypes) :-
    enum_types_(EnumsVals, Kind2Globals, VarTypes).

enum_types_([], [], []).
enum_types_([(VN, EV)|EVS], [TypeDef|TypeDefs], [(VN, TypeName)|VNS]) :-
    enum_names(EV, EVNames),
    intercalate(EVNames, ", ", EVNMS),
    format(atom(Y), '~w__T', [VN]),
    atom_string(Y, TypeName),
    format(atom(X), 'type ~w = enum { ~w };', [ TypeName, EVNMS ]),
    atom_string(X, TypeDef),
    enum_types_(EVS, TypeDefs, VNS).


enum_names([], []).
enum_names([(N,_)|ENS], [N|NS]) :- enum_names(ENS, NS).  % assumes numeric order.. but is that really important?

is_enum_val([(_,EN)|_], VName) :- enum_names(EN, ENames),
                                  member(VName, ENames),
                                  !.
is_enum_val([_|ENS], VName) :- is_enum_val(ENS, VName), !.

%% ----------------------------------------------------------------------

% Generate a set of mode specifications for each enumerated variable to enable
% kind2 defensive reachability and non-vacuity checking.
enum_modes(_, [], []).
enum_modes(Vars, [(N,VS)|EnumsVals], [[""|Modes]|ModeSpecs]) :-
    member(N, Vars),
    !,
    enum_mode(N, VS, Modes),
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
enum_mode(Var, [(N,_V)|VS], [Mode|Modes]) :-
    format(atom(Mode), '  mode ~w_~w ( require ~w = ~w; );', [Var, N, Var, N]),
    enum_mode(Var, VS, Modes).

%% ----------------------------------------------------------------------

% All Internal and Mode variables should always be defined.  They may not be used
% by the requirements in this CC, but they shouldn't hurt anything by being
% defined.

%% implicit_vars(Vars, Kind2Decls) :-
%%     implicit_vars_(Vars, Decls),
%%     intercalate(Vars, "\n", Decls).

implicit_vars(_, [], []).
implicit_vars(EnumVals, [V|VS], [D|DS]) :-
    get_dict(idType, V, "Internal"),
    get_dict(variable_name, V, VarName),
    \+ is_enum_val(EnumVals, VarName),
    !,
    get_dict(dataType, V, VarType),
    convert_type(VarType, KindType),
    get_dict(assignment, V, ValStr),
    string_trim(ValStr, Val),
    format(atom(D), "var ~w : ~w = ~w;", [ VarName, KindType, Val ]),
    implicit_vars(EnumVals, VS, DS).
% TODO: similar to above, but for Mode idType, and Val is modeRequirement.
% However, consider that Kind2 mode support is significantly more complex...
implicit_vars(EnumVals, [_|VS], DS) :- implicit_vars(EnumVals, VS, DS).

input_vars(VTypes, Reqs, Vars, Decls, Refs) :-
    input_vars_(VTypes, Reqs, Vars, Refs, Decls).
input_vars_(_, _, [], _, []).
input_vars_(VTypes, Reqs, [V|VS], [Name|SeenNames], Out) :-
    get_dict(idType, V, "Input"),  % filters out Internal, Mode, and Output vars
    get_dict(variable_name, V, Name),
    is_req_var(Name, Reqs),
    !,
    get_var_type(VTypes, Name, V, VarType),
    convert_type(VarType, KindType),
    format(atom(Decl), '~w:~w', [Name, KindType]),
    input_vars_(VTypes, Reqs, VS, SeenNames, DS),
    add_if_not_present(Name, Decl, SeenNames, DS, Out).
input_vars_(VTypes, Reqs, [_|VS], Seen, Out) :-
    input_vars_(VTypes, Reqs, VS, Seen, Out).


output_vars(VTypes, Vars, OutNames, Decls, Names) :-
    output_vars_(VTypes, Vars, OutNames, Names, Decls).
output_vars_(_, _, [], _, []).
output_vars_(VTypes, Vars, [VN|VNS], [Name|SeenNames], Out) :-
    find_named_var(VN, Vars, V),
    get_dict(idType, V, "Output"),  % filters out Internal and Mode vars
    !,
    get_dict(variable_name, V, Name),
    get_var_type(VTypes, Name, V, VarType),
    convert_type(VarType, KindType),
    format(atom(Decl), '~w:~w', [Name, KindType]),
    output_vars_(VTypes, Vars, VNS, SeenNames, DS),
    add_if_not_present(Name, Decl, SeenNames, DS, Out).
output_vars_(VTypes, Vars, [_|VNS], Seen, Out) :-
    output_vars_(VTypes, Vars, VNS, Seen, Out).

find_named_var(Name, VS, V) :- member(V, VS), get_dict(variable_name, V, Name).

get_var_type([(VName,VarType)|_], VName, _, VarType) :- !.
get_var_type([(VNameBase,VarType)|_], VName, _, VarType) :-
    scenarios_final_var_name(VNameBase, VName), !.
get_var_type([_|VTS], VName, V, VarType) :- get_var_type(VTS, VName, V, VarType).
get_var_type([], _, V, VarType) :- get_dict(dataType, V, VarType).

add_if_not_present(_, Decl, [], DS, [Decl|DS]).
add_if_not_present(Var, _, [Var|_], DS, DS) :- !.
add_if_not_present(Var, Decl, [_|VS], DS, Out) :-
    add_if_not_present(Var, Decl, VS, DS, Out).

convert_type("integer", "int") :- !.
convert_type("boolean", "bool") :- !.
convert_type(Other, Other).

req_internalvars([], [], []).
req_internalvars([R|RS], [G|GS], [D|DS]) :-
    get_dict(lando_req, R, LR),
    get_dict(req_name, LR, RID),
    normalize_kind2_var(RID, V),

    get_dict(fret_req, LR, Fretish),
    emit_fretish(Fretish, FT),

    get_dict(requirement, R, JReq),
    get_dict(semantics, JReq, Sem),
    get_dict('CoCoSpecCode', Sem, E),

    format(atom(D), '(* Req: ~w *)~n  var ~w : bool = ~w;~n', [ FT, V, E ]),
    format(atom(G), 'guarantee "~w" ~w;', [RID, V]),
    req_internalvars(RS, GS, DS).

is_req_var(VName, Reqs) :-
    member(Req, Reqs),
    get_dict(requirement, Req, JReq),
    get_dict(semantics, JReq, ReqSem),
    get_dict(variables, ReqSem, Vars),
    member(VName, Vars),
    !.

normalize_kind2_var(Inp, Out) :-
    subst('-', '_', Inp, Out).

%% ----------------------------------------------------------------------

kind2_validate(Context, Kind2File, OutDirectory, contract, ResultFile, Status) :-
    validate(Context, Kind2File, OutDirectory, "--enable CONTRACTCK", ResultFile, Status).
kind2_validate(Context, Kind2File, OutDirectory, model, ResultFile, Status) :-
    validate(Context, Kind2File, OutDirectory, "", ResultFile, Status).
validate(Context, Kind2File, OutDirectory, Args, ResultFile, Status) :-
    do_exec(Context, "kind2 lando fret validation",
            [ 'InpFile' = Kind2File,
              'OutDir' = OutDirectory,
              'Kind2Args' = Args,
              'JSONFile' = ResultFile ],
            %% [ "kind2 -json --enable CONTRACTCK --output_dir {OutDir} --timeout 60 {InpFile} > {JSONFile}"
            [ "kind2 -json {Kind2Args} --output_dir {OutDir} --timeout 60 {InpFile} > {JSONFile}"
              % --lus_strict
            ], [], ".", Sts),
    ( process_kind2_results(ResultFile, Sts, Status)
    ; Status = Sts
    ).

process_kind2_results(ResultFile, Sts, Status) :-
    open(ResultFile, read, ResultStrm),
    json_read_dict(ResultStrm, Results),
    show_kind2_results(Results, Sts, Status).

show_kind2_results([], Status, Status).
show_kind2_results([O|OS], Sts, Status) :-
    get_dict(objectType, O, OType),
    show_kind2_results(O, OType, Sts, Sts2),
    show_kind2_results(OS, Sts2, Status).

show_kind2_results(_, "kind2Options", Sts, Sts) :- !. % ignored
show_kind2_results(_, "analysisStart", Sts, Sts) :- !. % ignored
show_kind2_results(_, "analysisStop", Sts, Sts) :- !. % ignored
show_kind2_results(O, "log", Sts, Status) :-
    !,
    get_dict(level, O, Level),
    show_kind2_log(O, Level, Sts, Status).
show_kind2_results(O, "realizabilityCheck", Sts, Status) :-
    !,
    get_dict(result, O, Result),
    show_kind2_result(O, Result, Sts, Status).
show_kind2_results(O, "satisfiabilityCheck", Sts, Status) :-
    !,
    get_dict(result, O, Result),
    show_kind2_satresult(O, Result, Sts, Status).
show_kind2_results(O, "property", Sts, Status) :-
    !,
    get_dict(answer, O, Answer),
    get_dict(value, Answer, PropSts),
    show_kind2_result(O, PropSts, Sts, Status).

show_kind2_results(_, OType, Sts, BadSts) :-
    print_message(warning, unrecognized_kind2_result_type(OType)),
    succ(Sts, BadSts).


show_kind2_log(O, "error", Sts, BadSts) :-
    !,
    succ(Sts, BadSts),
    get_dict(source, O, Source),
    get_dict(file, O, File),
    get_dict(line, O, Line),
    get_dict(column, O, Col),
    get_dict(value, O, Msg),
    print_message(error, kind2_log_error(Source, File, Line, Col, Msg)).
show_kind2_log(_, _, Sts, Sts).  % All other log levels ignored


show_kind2_result(O, "realizable", Sts, Sts) :-
    !,
    get_dict(runtime, O, RT),
    get_dict(value, RT, Time),
    get_dict(unit, RT, Unit),
    print_message(success, kind2_realizable(Time, Unit)). % which CC?
show_kind2_result(O, "unrealizable", Sts, Sts) :-
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
    print_message(error, kind2_unrealizable(CSize, Names)).
show_kind2_result(O, "falsifiable", Sts, Sts) :-
    !,
    get_dict(counterExample, O, [CounterEx|OtherCounterEx]),
    get_dict(name, CounterEx, Name),
    get_dict(streams, CounterEx, Streams),
    findall(N, (member(Stream, Streams), trace_input(Stream, N)), Inputs),
    findall(N, (member(Stream, Streams), trace_output(Stream, N)), Outputs),
    append(Inputs, Outputs, InterestingVars),
    !,
    show_stream_steps(InterestingVars, Streams),
    print_message(error, kind2_falsifiable(Name)).
show_kind2_result(O, "reachable", Sts, Sts) :-
    !,
    get_dict(runtime, O, RT),
    get_dict(value, RT, Time),
    get_dict(unit, RT, Unit),
    get_dict(name, O, FullName),
    simple_varname(FullName, Name),
    print_message(success, kind2_reachable(Time, Unit, Name)). % which CC?
show_kind2_result(O, "unreachable", Sts, Sts) :-
    !,
    get_dict(name, O, FullName),
    simple_varname(FullName, Name),
    print_message(error, kind2_unreachable(Name)). % which CC?
show_kind2_result(O, "valid", Sts, Sts) :-
    !,
    get_dict(runtime, O, RT),
    get_dict(value, RT, Time),
    get_dict(unit, RT, Unit),
    get_dict(name, O, Name),
    print_message(success, kind2_valid(Time, Unit, Name)). % which CC?
show_kind2_result(_, R, Sts, Sts) :-
    print_message(error, unknown_kind2_result(R)).


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

show_kind2_satresult(O, "satisfiable", Sts, Sts) :-
    !,
    get_dict(runtime, O, RT),
    get_dict(value, RT, Time),
    get_dict(unit, RT, Unit),
    print_message(success, kind2_satisfiable(Time, Unit)). % which CC?
show_kind2_satresult(_, R, Sts, Sts) :-
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
    maplist(get_step_val(Streams, StepNum), Vars, Vals),
    !,
    maplist(string_max_length, ColSizes, Vals, UpdColSizes),
    succ(StepNum, NextStepNum),
    show_stream_steps(Vars, Streams, UpdColSizes, NextStepNum, Fmt, Lines).
show_stream_steps(_, _, ColSizes, _, Fmt, []) :-
    maplist(succ, ColSizes, SZS), % +1 on each for the space separator
    make_format([4|SZS], Fmt).


string_max_length(CurLen, Str, MaxLen) :-
        string_length(Str, MaxLen),
        MaxLen > CurLen,
        !.
string_max_length(MaxLen, _, MaxLen).

make_format([S|SZ], Fmt) :-
    make_format(SZ, SubFmt),
    format(atom(Fmt), '~w~d~w ~w', ['~w~t~', S, '+', SubFmt]).
make_format([], "~n").

get_step_val([S|_], StepNum, Var, Val) :-
    get_dict(name, S, Var),
    get_dict(instantValues, S, Vals),
    get_valnum(Vals, StepNum, RawVal),
    format(atom(V), '~w', [RawVal]),
    atom_string(V, Val).
get_step_val([_|Streams], StepNum, Var, Val) :-
    get_step_val(Streams, StepNum, Var, Val).

get_valnum(StepVals, StepNum, Val) :- member([StepNum,Val], StepVals).

prolog:message(kind2_realizable(0.0, _Unit)) -->
    % Suppress messages for instantly realizable elements: these are generally
    % the ancillary nodes and functions supporting the primary.  Another method
    % for detecting these would be to check the surrounding
    % analysisStart/analysisStop, except the realizable realizablityCheck outputs
    % are within those brackets and the unrealizable realizablityCheck is outside
    % of those brackets... why?
    [ ].
prolog:message(kind2_realizable(Time, Unit)) -->
    [ 'Realizable (~w ~w)' - [ Time, Unit ] ].
prolog:message(kind2_reachable(Time, Unit, Name)) -->
    [ 'Reachable (~w ~w): ~w' - [ Time, Unit, Name ] ].
prolog:message(kind2_valid(Time, Unit, Name)) -->
    [ 'Valid (~w ~w): ~w' - [ Time, Unit, Name ] ].
prolog:message(kind2_satisfiable(Time, Unit)) -->
    [ 'Satisfiable (~w ~w)' - [ Time, Unit ] ].
prolog:message(kind2_unrealizable(Num, Names)) -->
    [ 'UNREALIZABLE, ~w conflicts: ~w' - [ Num, Names ] ].
prolog:message(kind2_unreachable(Name)) -->
    [ 'UNREACHABLE: ~w' - [ Name ] ].
prolog:message(kind2_falsifiable(Name)) -->
    [ 'FALSIFIABLE: ~w' - [ Name ] ].
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

%% Called to generate an output file for kind2 contract analysis.
%
% EnumVals: list of values which are an enumeration
% Vars: all variable definitions
% CompName: the component name (used to name nodes and contracts)
% Reqs: all fret requirements
% CVars: the output variables for this contract
% Kind2: returns the Kind2 specification

reqs_to_kind2(EnumVals, Vars, CompName, Reqs, CVars, Kind2, FileNames) :-
    enum_types(EnumVals, Kind2Globals, VarTypes),
    !,

    enum_modes(CVars, EnumVals, ModeSpecs),
    append(ModeSpecs, AllModes),
    intercalate(AllModes, "\n", Modes),

    intercalate(Kind2Globals, "\n", GlobalDecls),
    implicit_vars(EnumVals, Vars, Kind2Decls),
    intercalate(Kind2Decls, "\n  ", NodeDecls),
    input_vars(VarTypes, Reqs, Vars, Kind2Input, Kind2Args),
    reverse(Kind2Input, RKind2Input),
    reverse(Kind2Args, RKind2Args),
    intercalate(RKind2Input, "; ", NodeArgDecls),
    intercalate(RKind2Args, ", ", NodeArgs),
    output_vars(VarTypes, Vars, CVars, Kind2OutputDecls, Kind2Outputs),
    intercalate(Kind2OutputDecls, "; ", NodeRet),
    intercalate(Kind2Outputs, ", ", NodeOutputs),
    req_internalvars(Reqs, Kind2Guarantees, Kind2ReqVars),
    intercalate(Kind2ReqVars, "\n  ", NodeReqDecls),
    intercalate(Kind2Guarantees, "\n  ", NodeGuarantees),

    cc_model_impls(CVars, Calls, FileNames),
    intercalate(Calls, "\n", NodeCalls),

    [NodeName] = [ CompName ],
    kind2_helpers(Helpers),
    Kind2 = {|string(NodeName,
                     NodeArgDecls, NodeArgs,
                     NodeRet, NodeOutputs,
                     NodeDecls,
                     NodeReqDecls,
                     NodeGuarantees,
                     GlobalDecls,
                     NodeCalls,
                     Modes,
                     Helpers)||
| {Helpers}
|
| {GlobalDecls}
|
| contract {NodeName}Spec( {NodeArgDecls} ) returns ( {NodeRet} );
| let
|   {Modes}
|
|   {NodeDecls}
|
|   {NodeReqDecls}
|
|   {NodeGuarantees}
|
| tel
|
| node {NodeName} ( {NodeArgDecls} ) returns ( {NodeRet} );
| (*@contract
|    import {NodeName}Spec({NodeArgs}) returns ({NodeOutputs});
| *)
| let
|   --%MAIN;
|   {NodeCalls}
| tel
|}.

%% ----------------------------------------------------------------------

cc_model_impls(OutVars, Calls, NodeImplFileNames) :-
    findall(I, cc_model_impl_name(OutVars, I), ImplNames),
    findall(C, cc_model_impl_call(ImplNames, C), Calls),
    findall(N, cc_model_impl_file(ImplNames, N), NodeImplFileNames).

cc_model_impl_name(OutVars, ImplName) :-
    eng:eng(system, model, kind2, ImplName, output, IOutVars),
    % n.b. expect a single output variable, but allow multiples separated by
    % commas here.
    split_string(IOutVars, ",", "", VS),
    findall(V, (member(V, VS), member(V, OutVars)), NVS),
    length(VS, VSLen),
    length(NVS, VSLen).  % lengths are the same

cc_model_impl_call(ImplNames, Call) :-
    member(NodeName, ImplNames),
    eng:eng(system, model, kind2, NodeName, output, OutArg),
    eng:eng(system, model, kind2, NodeName, inputs, InpArgs),
    format(atom(Call), '  ~w = ~w(~w);', [ OutArg, NodeName, InpArgs ]).

cc_model_impl_file(ImplNames, FName) :-
    member(NodeName, ImplNames),
    eng:eng(system, model, kind2, NodeName, file, FName).


%% ----------------------------------------------------------------------

kind2_helpers(Helpers) :-
    Helpers = {|string||
| --Historically: X has always been true
| -- As soon as X is false once, Y will be false forever
| -- (falling edge)
| node H(X:bool) returns (Y:bool);
| let
|     Y = X -> (X and (pre Y));
| tel
|
| --Y since inclusive X
| --  Y is enabler/reset: while Y, from X true onwards
| -- or
| --  at X, Y until false, then reset
| node SI(X,Y: bool) returns (Z:bool);
| let
| Z = Y and (X or (false -> pre Z));
| tel
|
| --Y since X
| -- X is true, then Y until false
| node S(X,Y: bool) returns (Z:bool);
| let
| Z = X or (Y and (false -> pre Z));
| tel
|
| --Once
| --  the first time X is true, Y is true forever
| --  (rising edge)
| node O(X:bool) returns (Y:bool);
| let
|  Y = X or (false -> pre Y);
| tel
|
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
| --Timed Once: general case
| -- True R ticks after each X first true until L ticks after X is last true
| node OT( L: int;  R: int; X: bool) returns (Y: bool);
| var  D:bool;
| let
|   D=delay(X, R);
|   Y=OTlore(L-R, D);
| tel
|
| -- Timed Historically: general case
| -- True if X has been true, and for R ticks afterwards, false thereafter
| -- Always true for at least R ticks of the timeline, even if X is never true
| -- L is ignored
| node HT( L: int;  R: int; X: bool) returns (Y: bool);
| let
|   Y = not OT(L, R, not X);
| tel
|
| -- Timed Since: general case
| -- R ticks after X is true, for L ticks if/while Y remains true
| node ST( L: int;  R: int; X: bool; Y: bool)  returns (Z: bool);
| let
|   Z = S(X, Y) and OT(L, R, X);
| tel
|
| -- Timed Since Inclusive: general case
| -- R ticks after X is true, for L ticks if X or while Y remains true
| node SIT( L: int;  R: int; X: bool; Y: bool) returns (Z: bool);
| let
|   Z = SI(X, Y) and OT(L, R, X);
| tel
|
| -- Pre for integers, with an initial value at FTP
| node preInt(InitialValue, X: int) returns (Y:int);
| let
|   Y = InitialValue -> pre X;
| tel
|
| -- Pre for reals, with an initial value at FTP
| node preReal(InitialValue, X: real) returns (Y:real);
| let
|   Y = InitialValue -> pre X;
| tel
|
| -- Pre for booleans, with an initial value at FTP
| node preBool(InitialValue, X: bool) returns (Y:bool);
| let
|   Y = InitialValue -> pre X;
| tel
|
| -- The equivalent of LTL's Y in Lustre.
| --   Initially false, then the previous value of X (false, delay X by one)
| node YtoPre(X: bool) returns (Y:bool);
| let
|   Y = false -> pre X;
| tel
|
| -- The equivalent of LTL's Z in Lustre.
| --   Initially true, then the previous value of X (true, delay X by one)
| node ZtoPre(X: bool) returns (Y:bool);
| let
|   Y = true -> pre X;
| tel
|
| -- Absolute value for reals
| function absReal(x:real) returns(y: real);
| let
|   y = if (x >= 0.0) then x else -x;
| tel
|
| -- Absolute value for integers
| function absInt(x:int) returns(y: int);
| let
|   y = if (x >= 0) then x else -x;
| tel
|
| -- Maximum value between two reals
| function maxReal (a : real; b : real)
| returns (z : real);
| let
|   z = (if (((a) >= (b))) then (a) else (b));
| tel
|
| -- Maximum value between two integers
| function maxInt (a : int; b : int)
| returns (z : int);
| let
|   z = (if (((a) >= (b))) then (a) else (b));
| tel
|
| -- Minimum value between two integers
| function minInt (a : int; b : int)
| returns (z : int);
| let
|   z = (if (((a) <= (b))) then (a) else (b));
| tel
|
| -- Minimum value between two reals
| function minReal (a : real; b : real)
| returns (z : real);
| let
|   z = (if (((a) <= (b))) then (a) else (b));
| tel
|
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
|}.
