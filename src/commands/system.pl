:- module(system_eng, [ system_cmd/3, system_focus/1,
                        system_help/1, system_help/2 ]).
% n.b. the module name cannot be "system" because that is reserved in SWI Prolog.

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(strings)).
:- use_module('../englib').
:- use_module(exec_subcmds).
:- use_module('../load').
:- use_module('../datafmts/frettish').
:- use_module('../datafmts/lando').
:- use_module('../lando_tool').
:- use_module('../lando_validate').
:- use_module('../lando_fret').
:- use_module('../fret_kind2').

:- dynamic system_spec/1.


system_focus("Systems Engineering").

system_help(Info) :-
    engfile_dir(EngDirV),
    setof(F, X^Y^spec_output_type(F, X, Y), FS),
    intercalate(["ALL"|FS], " | ", Formats),
    [EngDir, OutFormats] = [ EngDirV, Formats ],
    Info = {|string(EngDir, OutFormats)||
| Perform a SYSTEMS engineering task.
|
| Systems engineering involves the specification of the components and
| their relationships with this system.  It also associates those components
| with the system requirements.  System requirements can be validated for
| consistency, completeness, and lack of conflicts, and outputs from the
| system specification can be generated.
|
| System-level information is specified in one or more .eng files in EQIL
| format in the {EngDir} directory of the project.
|
|   system =
|     spec =
|       SPECIFICATION =
|         name = NAME OF SPECIFICATION
|         file = TOP_LEVEL_RELATIVE_FILENAME(S)
|         format = { lando | other }
|         generate =
|           OUTPUT_FILENAME
|             format = {OutFormats}
|
|     model =
|       kind2 =
|         KIND2_NODE_NAME =
|           output = OUTPUT_VAR(S)
|           inputs = INPUT_VARS
|           file = KIND2_NODE_FILENAME
|
|  There may be multiple model files.  The file specifies a Lustre file
|  containing a node with KIND2_NODE_NAME that takes the specified comma-
|  separated list of inputs and generates the named output (no types).
|  When the output and inputs provided are a subset of the inputs and
|  outputs of the contract node, then a call to the KIND2_NODE_NAME will
|  be generated in the output top-level contract file, and the contents
|  of the KIND2_NODE_FILENAME will be appended to that top-level file.
|}.

system_help(list, "List project's system specifications").
system_help(validate, "Check validity of system specifications").
system_help(gen, "Generate system specification outputs (e.g. lando to JSON)").

system_known_specification_types([ "lando" ]).

system_cmd(_, ['list'|_], 0) :-
    writeln('Known system specifications:'), show_system_specs(_); true.

system_cmd(_, ['validate'], specify_ssl_id) :- !.
system_cmd(Context, ['validate', 'ALL'], Result) :-
    process_system_specs(Context, validate_system_spec, Result), !.
system_cmd(Context, ['validate'|Specs], Result) :-
    process_system_specs(Context, validate_system_spec, Specs, Result), !.

system_cmd(_, ['gen'], specify_ssl_id) :- !.
system_cmd(Context, ['gen', 'ALL'], Result) :-
    process_system_specs(Context, generate_system_spec, Result), !.
system_cmd(Context, ['gen'|Specs], Result) :-
    process_system_specs(Context, generate_system_spec, Specs, Result), !.

system_cmd(_, [Cmd|_], invalid_subcmd(system, Cmd)).

% ----------------------------------------------------------------------

process_system_specs(Context, Op, Result) :-
    Context = context(_, TopDir),
    working_directory(_, TopDir),
    (setof(S, eng:key(system, spec, S), Specs); Specs = []),
    maplist(call(Op, Context), Specs, Result).

process_system_specs(Context, Op, Specs, Result) :-
    Context = context(_, TopDir),
    working_directory(_, TopDir),
    findall(R, (member(Spec, Specs), call(Op, Context, Spec, R)), Result).

prolog:message(specify_ssl_id) -->
    [ 'Please specify the system specification to process (or "ALL")~n' - [] ].
prolog:message(invalid_system_spec(Spec)) -->
    [ 'Validation failed for system specification "~w"~n' - [ Spec ] ].
prolog:message(unknown_spec_format(Spec, Format)) -->
    [ 'Unknown system specification format (~w) for spec "~w"~n' - [ Format,
                                                                     Spec ] ].
prolog:message(spec_format_not_specified(Spec)) -->
    [ 'No format specified for system specification "~w"~n' - [ Spec ] ].
prolog:message(no_spec_file(Spec)) -->
    [ 'No file provided for system specification "~w".~n' - [ Spec ] ].
prolog:message(unknown_spec(Spec)) -->
    [ 'Unknown system specification "~w"; see "eng system list".~n' - [ Spec ] ].

find_spec(Input, Input) :- eng:key(system, spec, Input), !.
find_spec(Input, Spec) :- atom_string(Input, InpStr),
                          eng:eng(system, spec, Spec, name, InpStr), !.

with_spec_format_and_file(InputSpec, Op, R) :-
    find_spec(InputSpec, Spec), !,
    with_spec_format_and_file_(Spec, Op, R).
with_spec_format_and_file(Spec, _, unknown_spec(Spec)).

with_spec_format_and_file_(Spec, Op, R) :-
    eng:eng(system, spec, Spec, format, Format), !,
    with_spec_format_and_file_(Spec, Op, Format, R).
with_spec_format_and_file_(Spec, _, 1) :-
    print_message(error, spec_format_not_specified(Spec)).

with_spec_format_and_file_(Spec, Op, Format, R) :-
    system_known_specification_types(Known),
    member(Format, Known), !,
    with_spec_format_and_file_(Spec, Op, Format, known, R).
with_spec_format_and_file_(Spec, _, Format, unknown_spec_format(Spec, Format)).

with_spec_format_and_file_(Spec, Op, Format, known, R) :-
    eng:eng(system, spec, Spec, file, SpecFile), !,
    ( call(Op, Spec, Format, SpecFile, R), !
    ; print_message(error, invalid_system_spec(Spec)), R = 1
    ).
with_spec_format_and_file_(Spec, _, _, _, 1) :-
    print_message(error, no_spec_file(Spec)).

% ----------------------------------------------------------------------

show_system_specs(Spec) :-
    eng:key(system, spec, Spec),
    show_system_spec(Spec).

show_system_spec(Spec) :-
    eng:eng(system, spec, Spec, name, Name),
    show_system_spec(Spec, Name).
show_system_spec(Spec) :-
    \+ eng:key(system, spec, Spec, name),
    show_system_spec(Spec, "").

show_system_spec(Spec, Name) :-
    eng:eng(system, spec, Spec, format, Format),
    show_system_spec(Spec, Name, Format).
show_system_spec(Spec, Name) :-
    \+ eng:key(system, spec, Spec, format),
    show_system_spec(Spec, Name, "?fmt?").

show_system_spec(Spec, Name, Format) :-
    format('  [~w] ~w: ~w~n', [ Format, Name, Spec ]),
    fail. % backtrack and try the next one

% ----------------------------------------------------------------------

validate_system_spec(Context, InputSpec, R) :-
    with_spec_format_and_file(InputSpec, validate_spec(Context), R).

validate_spec(Context, Spec, "lando", SpecFile, R) :-
    parse_lando_file(SpecFile, SSL), !,
    validate_lando(SSL, Problems),
    length(Problems, NumProbs),
    ( Problems == []
    -> validate_lando_fret(Context, Spec, SSL, R), !
    ; maplist(show_lando_validation_error(Spec, SpecFile), Problems),
      print_message(error, lando_validation_errors(Spec, SpecFile, NumProbs)),
      R = NumProbs
    ).
validate_spec(_, Spec, Format, SpecFile, 1) :-
    print_message(error, invalid_parse(Format, Spec, SpecFile)).
validate_spec(_, _, _, _, 1).

validate_lando_fret(Context, Spec, SSL, R) :-
    ( eng:eng(system, spec, Spec, generate, Dir, format, "fret_kind2")
    ; Dir = "fret_kind2"
    ),
    ensure_dir(Dir),
    !,
    % if no FRET, the next should not succeed and this predicate is false
    lando_to_fret(SSL, Reqs, FretVars, SrcRefs),
    write_fret_kind2(Dir, SSL, Reqs, FretVars, OutFiles),

    assertz(generated(gid(1), thing(SSL, form(mem, lando_SSL)))),
    assertz(generated(gid(2), thing(Reqs, form(mem, fretments)), gid(1))),
    assertz(generated(gid(3), thing(FretVars, form(mem, fret_variables)), gid(1))),
    assertz(generated(gid(4), thing(SrcRefs, form(mem, fret_srcrefs)), gid(1))),

    validate_fret_results(Context, Spec, OutFiles, Results),
    process_kind2_results(Results, R).

validate_fret_results(Context, Spec, OutFiles, Results) :- % KWQ: version for which there is no RACK generate!  And rename generate to something else since this no longer comes from "eng system gen".
    eng:key(system, spec, Spec, generate, OutFile),
    eng:eng(system, spec, Spec, generate, OutFile, format, "RACK"),
    !,
    ensure_file_loc(OutFile),
    open(OutFile, write, OutStrm),
    format(OutStrm, '~w,~w,~w,~w,~w,~w,~w~n',
           [ "Project", "Component", "Requirement", "Source", "FretID",
             "Status", "Frettish" ]),
    asserta((output_kind2_result(ReqName, Level, Msg) :-
                 output_kind2_rack_csv(OutStrm, ReqName, Level, Msg),
                 fail),  % fail to next output_kind2_result (print_message below)
            RACK_out),
    maplist(validate_lando_fret_cc(Context), OutFiles, Results),
    erase(RACK_out).
validate_fret_results(Context, Spec, OutFiles, Results) :-
    maplist(validate_lando_fret_cc(Context), OutFiles, Results).

output_kind2_rack_csv(OutStrm, ReqName, _Level, Msg) :-
    generated(gid(G1), thing(Reqs, form(mem, fretments)), gid(1)),
    find_req(ReqName, Reqs, R),
    %% If not found, probably a mode req or the _one_mode_active synthetic
    %% target.  For now, these are ignored, but enable this instead to get more
    %% details.
    %%
    %% (find_req(ReqName, Reqs, R), !
    %% ; format('no req for ~w~n~n~n', [ReqName]),
    %%   fail),
    generated(gid(G2), thing(SrcRefs, form(mem, fret_srcrefs)), gid(1)),
    get_srcrefs_for_req(SrcRefs, R, SR),
    (SR == [] -> SRS = [srcref("", "")] ; SRS = SR),
    maplist(ww_csv(OutStrm, R, Msg), SRS, _).

find_req(RName, [R|_], R) :- get_dict(lando_req, R, LR),
                             get_dict(req_name, LR, RName).
find_req(RName, [R|_], R) :- get_dict(lando_req, R, LR),
                             atom_string(RName, RNS),
                             get_dict(req_name, LR, RNS).
find_req(RName, [R|_], R) :- get_dict(lando_req, R, LR),
                             atom_string(RNA, RName),
                             get_dict(req_name, LR, RNA).
find_req(RName, [_|RS], R) :- find_req(RName, RS, R).

get_srcrefs_for_req(SrcRefs, R, SR) :-
    get_dict(lando_req, R, X),
    get_dict(req_id, X, RID),
    findall(Y, member(reqsrc(RID, Y), SrcRefs), SR).

ww_csv(OutStrm, R, Msg, srcref(ReqTag, ReqSrc), ReqTag) :-
    get_dict(lando_req, R, LR),
    get_dict(req_project, LR, P),
    get_dict(req_name, LR, I),
    get_dict(fret_req, LR, Fretment),
    Fretment = fretment(_, _, component_info(Comp), _, _),
    get_dict(component, Comp, C),
    emit_fretish(Fretment, T),
    kind2_result_for_RACK(Msg, CVal),
    format(OutStrm, '~w,~w,~w,~w,~w,~w,"~w"~n',
           [P, C, ReqTag, ReqSrc, I, CVal, T]).

kind2_result_for_RACK(kind2_valid(_, _, _), "VALID") :- !.
kind2_result_for_RACK(kind2_reachable(_), "REACHABLE") :- !.
kind2_result_for_RACK(kind2_reachable_unexpectedly(_), "REACHABLE") :- !.
kind2_result_for_RACK(kind2_unreachable(_), "UNREACHABLE") :- !.
kind2_result_for_RACK(kind2_unreachable_expected(_), "UNREACHABLE") :- !.
kind2_result_for_RACK(Msg, "UNKNOWN") :-
    format('Unknown kind2 result for RACK: ~w~n', [Msg]).

prolog:message(kind2_good(NPass)) -->
    ["TOTAL: PASS  (~w checks)~n" - [ NPass ] ].
prolog:message(kind2_good_with_exp_failures(NPass, NExpFailures)) -->
    { NTot is NPass + NExpFailures },
    ["TOTAL: PASS  (~w checks | ~w passed, ~w FAILED AS EXPECTED)~n"
     - [ NTot, NPass, NExpFailures ]].
prolog:message(kind2_good_with_unexp_good(NPass, NUnexpPass)) -->
    { NTot is NPass + NUnexpPass },
    ["TOTAL: PASS  (~w checks | ~w passed (~w of those UNEXPECTEDLY))~n"
     - [ NTot, NTot, NUnexpPass ]].
prolog:message(kind2_good_with_unexpected(NPass, NExpFail, NUnexpPass)) -->
    { NTot is NPass + NExpFail + NUnexpPass,
      NGood is NPass + NUnexpPass
    },
    ["TOTAL: PASS  (~w checks | ~w passed (~w of those UNEXPECTEDLY), ~w FAILED AS EXPECTED)~n"
     - [ NTot, NGood, NUnexpPass, NExpFail ]].
prolog:message(kind2_failures(0, NPass, NFail, 0, 0)) -->
    { NTot is NPass + NFail }, !,
    ["TOTAL: ~w FAILED  (~w checks | ~w passed)~n"
     - [NFail, NTot, NPass] ].
prolog:message(kind2_failures(NInv, NPass, NFail, 0, 0)) -->
    { NTot is NInv + NPass + NFail },
    ["TOTAL: ~w+~w FAILED  (~w checks | ~w passed)~n"
     - [NFail, NInv, NTot, NPass] ].
prolog:message(kind2_failures(0, NPass, NFail, 0, NUnexpPass)) -->
    { NTot is NPass + NFail + NUnexpPass,
      NGood is NPass + NUnexpPass
    }, !,
    ["TOTAL: ~w FAILED  (~w checks | ~w passed (~w of those UNEXPECTEDLY)~n"
     - [NFail, NTot, NGood, NUnexpPass] ].
prolog:message(kind2_failures(NInv, NPass, NFail, 0, NUnexpPass)) -->
    { NTot is NInv + NPass + NFail + NUnexpPass,
      NGood is NPass + NUnexpPass
    },
    ["TOTAL: ~w+~w FAILED  (~w checks | ~w passed (~w of those UNEXPECTEDLY)~n"
     - [NFail, NInv, NTot, NGood, NUnexpPass] ].
prolog:message(kind2_failures(0, NPass, NFail, NExpFail, 0)) -->
    { NTot is NPass + NFail + NExpFail }, !,
    ["TOTAL: ~w FAILED  (~w checks | ~w passed, ~w FAILED AS EXPECTED)~n"
     - [NFail, NTot, NPass, NExpFail] ].
prolog:message(kind2_failures(NInv, NPass, NFail, NExpFail, 0)) -->
    { NTot is NInv + NPass + NFail + NExpFail },
    ["TOTAL: ~w+~w FAILED  (~w checks | ~w passed, ~w FAILED AS EXPECTED)~n"
     - [NFail, NInv, NTot, NPass, NExpFail] ].
prolog:message(kind2_failures(0, NPass, NFail, NExpFail, NUnexpPass)) -->
    { NTot is NPass + NFail + NExpFail + NUnexpPass,
      NGood is NPass + NUnexpPass
    }, !,
    ["TOTAL: ~w FAILED  (~w checks | ~w passed (~w of those UNEXPECTEDLY), ~w FAILED AS EXPECTED)~n"
     - [NFail, NTot, NGood, NUnexpPass, NExpFail] ].
prolog:message(kind2_failures(NInv, NPass, NFail, NExpFail, NUnexpPass)) -->
    { NTot is NInv + NPass + NFail + NExpFail + NUnexpPass,
      NGood is NPass + NUnexpPass
    },
    ["TOTAL: ~w+~w FAILED  (~w checks | ~w passed (~w of those UNEXPECTEDLY), ~w FAILED AS EXPECTED)~n"
     - [NFail, NInv, NTot, NGood, NUnexpPass, NExpFail] ].

process_kind2_results(Results, R) :-
    append(Results, All),
    process_kind2_results(All, _{ninv:0,
                                 npass:0,
                                 nfail:0,
                                 nfailAsExp:0,
                                 nUnexpPass:0}, R).

process_kind2_results([], Stats, 0) :-
    get_dict(ninv, Stats, 0),
    get_dict(nfail, Stats, 0),
    get_dict(nfailAsExp, Stats, 0),
    get_dict(nUnexpPass, Stats, 0),
    !,
    get_dict(npass, Stats, NPass),
    print_message(success, kind2_good(NPass)).
process_kind2_results([], Stats, 0) :-
    get_dict(ninv, Stats, 0),
    get_dict(nfail, Stats, 0),
    get_dict(nUnexpPass, Stats, 0),
    !,
    get_dict(npass, Stats, NPass),
    get_dict(nfailAsExp, Stats, NFailAsExp),
    print_message(warning, kind2_good_with_exp_failures(NPass, NFailAsExp)).
process_kind2_results([], Stats, 0) :-
    get_dict(ninv, Stats, 0),
    get_dict(nfail, Stats, 0),
    get_dict(nfailAsExp, Stats, 0),
    !,
    get_dict(npass, Stats, NPass),
    get_dict(nUnexpPass, Stats, NUnexpPass),
    print_message(warning, kind2_good_with_unexp_good(NPass, NUnexpPass)).
process_kind2_results([], Stats, 0) :-
    get_dict(ninv, Stats, 0),
    get_dict(nfail, Stats, 0),
    !,
    get_dict(npass, Stats, NPass),
    get_dict(nfailAsExp, Stats, NFailAsExp),
    get_dict(nUnexpPass, Stats, NUnexpPass),
    print_message(warning, kind2_good_with_unexpected(NPass, NFailAsExp,
                                                      NUnexpPass)).
process_kind2_results([], Stats, 0) :-
    !,
    get_dict(nfail, Stats, NFail),
    get_dict(ninv, Stats, NInv),
    get_dict(npass, Stats, NPass),
    get_dict(nfailAsExp, Stats, NFailAsExp),
    get_dict(nUnexpPass, Stats, NUnexpPass),
    print_message(error, kind2_failures(NInv, NPass, NFail, NFailAsExp,
                                        NUnexpPass)).

process_kind2_results([ignored|KS], Stats, R) :-
    !,
    process_kind2_results(KS, Stats, R).
process_kind2_results([K|KS], Stats, R) :-
    stats_field(K, F),
    get_dict(F, Stats, V),
    succ(V, U),
    put_dict(F, Stats, U, UpdStats),
    process_kind2_results(KS, UpdStats, SR),
    update_validate_error(K, SR, R).
process_kind2_results([K|_],S,99) :-
    \+ stats_field(K, _),
    format('ERR: unhandled kind2 result: ~w~n  stats = ~w~n~n', [K, S]),
    fail.


update_validate_error(Res, PrevCnt, Cnt) :-
    is_validate_error(Res),
    succ(PrevCnt, Cnt).
update_validate_error(Res, Cnt, Cnt) :-
    \+ is_validate_error(Res).

is_validate_error(invalid).
is_validate_error(failed).
% is_validate_error(unexpectedly_passed).

stats_field(invalid, ninv).
stats_field(passed, npass).
stats_field(failed, nfail).
stats_field(failed_as_expected, nfailAsExp).
stats_field(unexpectedly_passed, nUnexpPass).
stats_field(satisfiable, npass).

validate_lando_fret_cc(Context, contract(OutFile), Status)  :-
    validate_lando_fret_cc(Context, contract, OutFile, Status).
validate_lando_fret_cc(Context, model(OutFile), Status) :-
    validate_lando_fret_cc(Context, model, OutFile, Status).
validate_lando_fret_cc(Context, Kind2Mode, OutFile, Status) :-
    directory_file_path(OutD, _, OutFile),
    %absolute_file_name(OutFile, OutFilePath),
    file_name_extension(OutBase, 'lus', OutFile),
    string_concat(OutBase, "_result", ResultBase),
    file_name_extension(ResultBase, 'json', ResultFile),
    kind2_validate(Context, OutFile, OutD, Kind2Mode, ResultFile, Status).

show_lando_validation_error(Spec, SpecFile, Err) :-
    print_message(error, lando_validation_error(Spec, SpecFile, Err)).

prolog:message(invalid_parse(Format, Spec, SpecFile)) -->
    [ 'Validation Error: unable to parse ~w format~n  [~w = ~w]~n' -
      [ Format, SpecFile, Spec ] ].
prolog:message(lando_validation_error(Spec, SpecFile, Err)) -->
    [ 'Validation Error: ~w~n  [~w = ~w]~n' - [ Err, SpecFile, Spec ] ].
prolog :message(number_lando_validation_errors(Spec, SpecFile, Count)) -->
    [ 'Total of ~w validation Errors~n  [~w = ~w]~n' -
      [ Count, SpecFile, Spec ] ].


% ----------------------------------------------------------------------

generate_system_spec(Context, InputSpec, R) :-
    with_spec_format_and_file(InputSpec, generate_spec(Context), R).

generate_spec(Context, Spec, "lando", SpecFile, Result) :-
    parse_lando_file(SpecFile, SSL), !,
    findall(R, generate_spec_outputs(Context, Spec, "lando", SSL, R), Results),
    sum_list(Results, Result).
generate_spec(_, _, _, _, 1).

generate_spec_outputs(_, Spec, "lando", SSL, Result) :-
    spec_output_type(OutType, OutName, OutFunc),
    eng:key(system, spec, Spec, generate, OutFile),
    eng:eng(system, spec, Spec, generate, OutFile, format, OutType),
    ensure_file_loc(OutFile),
    open(OutFile, write, OutStrm),
    (call(OutFunc, OutStrm, SSL)
    -> Result = 0,
       print_message(information, wrote_lando_as(OutName, Spec, OutFile))
    ; Result = 1,
      print_message(error, did_not_write(Spec, OutFile, OutName))
    ).
generate_spec_outputs(_, Spec, "lando", SSL, Result) :-
    eng:key(system, spec, Spec, generate, OutDir),
    % This is an output directory, not an output file
    eng:eng(system, spec, Spec, generate, OutDir, format, "fret_kind2"),
    retractall(fret_kind2:kind2_disallow_enums),
    (write_lando_fret_kind2(OutDir, SSL, OutFiles)
    -> Result = 0,
       wrote_file_messages(Spec, "fret_kind2", OutFiles)
    ; Result = 1,
      print_message(error, did_not_write(Spec, OutDir, "fret_kind2"))
    ).
generate_spec_outputs(Context, Spec, "lando", SSL, Result) :-
    eng:key(system, spec, Spec, generate, OutDir),
    % This is an output directory, not an output file
    eng:eng(system, spec, Spec, generate, OutDir, format, "test_traces"),
    % Test traces and the oracle cannot handle explicit enums, so disable them,
    % forcing this kind2 output to use constrained integers instead.
    asserta(fret_kind2:kind2_disallow_enums),
    % If there is a model, then the oracle will use the SUT output as "input of
    % the output" to each oracle evaluation, which may be validated against
    % initial guarantees (e.g. of output range for enums), but then the model
    % will be used to calculate a new output value and that model's output will
    % be validated against all the contracts, rather than the SUT output.
    % Therefore, suppress inclusion of the model in the kind2 output here.
    asserta(fret_kind2:kind2_no_model),
    delete_directory_contents(OutDir),
    (write_lando_fret_kind2(OutDir, SSL, OutFiles)
    -> wrote_file_messages(Spec, "fret_kind2", OutFiles),
       retractall(fret_kind2:kind2_disallow_enums),
       retractall(fret_kind2:kind2_no_model),
       kind2_gen_traces(Context, OutDir, OutFiles, Result)
    ; Result = 1,
      retractall(fret_kind2:kind2_disallow_enums),
      retractall(fret_kind2:kind2_no_model),
      print_message(error, did_not_write(Spec, OutDir, "fret_kind2"))
    ).
kind2_gen_traces(_, _, [], 0).
kind2_gen_traces(Context, OutDir, [contract(IFile)|IFiles], Result) :-
    kind2_gen_traces(Context, OutDir, IFile, IFiles, Result).
kind2_gen_traces(Context, OutDir, [model(IFile)|IFiles], Result) :-
    kind2_gen_traces(Context, OutDir, IFile, IFiles, Result).
kind2_gen_traces(Context, OutDir, IFile, IFiles, Result) :-
    do_exec(Context, "kind2 test generation",
            [ 'OutDir' = OutDir,
              'InpFile' = IFile
            ],
            [ "kind2 --testgen true -q --exit_code_mode only_errors --output_dir {OutDir} {InpFile}" ],
            [], ".", Sts),
    % Will report some guarantees are invalid because there is no model (see
    % above for why there is no model when generating test traces), but
    % exit_code_mode only_errors ensures that sts will only be non-zero if the
    % test traces and oracle generation fail.
    kind2_gen_traces(Context, OutDir, IFiles, SubResult),
    Result is Sts + SubResult.

spec_output_type("json", "JSON", write_lando_json).
spec_output_type("markdown", "Markdown", write_lando_markdown).
spec_output_type("fret", "FRET", write_lando_fret).
spec_output_type("fret-summary", "FRET Summary", write_lando_fret_summary).


wrote_file_messages(_, _, []).
wrote_file_messages(Spec, Kind, [contract(OutFile)|FS]) :-
    string_concat(Kind, " contract", FKind),
    print_message(information, wrote_lando_as(FKind, Spec, OutFile)),
    wrote_file_messages(Spec, Kind, FS).
wrote_file_messages(Spec, Kind, [model(OutFile)|FS]) :-
    string_concat(Kind, " model", FKind),
    print_message(information, wrote_lando_as(FKind, Spec, OutFile)),
    wrote_file_messages(Spec, Kind, FS).

prolog:message(wrote_file(Spec, OutFile, Kind)) -->
    [ 'Wrote lando spec "~w" to ~w file ~w~n' - [ Spec, Kind, OutFile ]].
prolog:message(did_not_write(_Spec, OutFile, Kind)) -->
    [ 'Unable to write ~w to file ~w due to errors~n' - [ Kind, OutFile ]].
