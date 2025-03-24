:- module(system_eng, [ system_cmd/3, system_focus/1,
                        system_help/1, system_help/2 ]).
% n.b. the module name cannot be "system" because that is reserved in SWI Prolog.

:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(strings)).
:- use_module('../englib').
:- use_module('exec_subcmds').
:- use_module('../load').
:- use_module('../datafmts/lando').
:- use_module('../lando_tool').
:- use_module('../lando_validate').

:- dynamic system_spec/1.


system_focus("Systems Engineering").

system_help(Info) :-
    engfile_dir(EngDirV),
    [EngDir] = [ EngDirV ],
    Info = {|string(EngDir)||
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
|         file = TOP_LEVEL_RELATIVE_FILENAME
|         format = { lando | other }
|         generate =
|           OUTPUT_FILENAME
|             format = { ALL | fret | json | markdown }
|
|}.

system_help(list, "List project's system specifications").
system_help(validate, "Check validity of system specifications").
system_help(gen, "Generate system specification outputs (e.g. lando to JSON)").

system_known_specification_types([ "lando" ]).

system_cmd(_, ['list'|_], 0) :-
    writeln('Known system specifications:'), show_system_specs(_); true.

system_cmd(_, ['validate'], 1) :- print_message(error, specify_ssl_id).
system_cmd(Context, ['validate', 'ALL'], Result) :-
    process_system_specs(Context, validate_system_spec, Result).
system_cmd(Context, ['validate'|Specs], Result) :-
    process_system_specs(Context, validate_system_spec, Specs, Result).

system_cmd(_, ['gen'], 1) :- print_message(error, specify_ssl_id).
system_cmd(Context, ['gen', 'ALL'], Result) :-
    process_system_specs(Context, generate_system_spec, Result).
system_cmd(Context, ['gen'|Specs], Result) :-
    process_system_specs(Context, generate_system_spec, Specs, Result).

system_cmd(_, [Cmd|_], 1) :-
    print_message(error, invalid_system_subcmd(Cmd)).

% ----------------------------------------------------------------------

process_system_specs(Context, Op, Result) :-
    Context = context(_, TopDir),
    working_directory(_, TopDir),
    (setof(S, eng:key(system, spec, S), Specs); Specs = []),
    maplist(call(Op, Context), Specs, Results),
    sum_list(Results, Result).

process_system_specs(Context, Op, Specs, Result) :-
    Context = context(_, TopDir),
    working_directory(_, TopDir),
    findall(R, (member(Spec, Specs), call(Op, Context, Spec, R)), Results),
    sum_list(Results, Result).

prolog:message(invalid_system_subcmd(Cmd)) -->
    [ 'Invalid "system" subcommand: ~w~n' - [ Cmd ] ],
    { known_subcommands(system, CS) },
    [ 'Valid sub-commands: ~w~n' - [ CS ] ].
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
with_spec_format_and_file(Spec, _, 1) :-
    print_message(error, unknown_spec(Spec)).

with_spec_format_and_file_(Spec, Op, R) :-
    eng:eng(system, spec, Spec, format, Format), !,
    with_spec_format_and_file_(Spec, Op, Format, R).
with_spec_format_and_file_(Spec, _, 1) :-
    print_message(error, spec_format_not_specified(Spec)).

with_spec_format_and_file_(Spec, Op, Format, R) :-
    system_known_specification_types(Known),
    member(Format, Known), !,
    with_spec_format_and_file_(Spec, Op, Format, known, R).
with_spec_format_and_file_(Spec, _, Format, 1) :-
    print_message(error, unknown_spec_format(Spec, Format)).

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
    -> format('Validated Lando specification ~w in ~w~n', [Spec, SpecFile]),
       (validate_lando_fret(Context, Spec, SSL, R) ; R = 0)
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
    (exists_directory(Dir) ; make_directory(Dir)),
    !,
    % if no FRET, the next should not succeed and this predicate is false
    write_lando_fret_kind2(Dir, SSL, OutFiles),
    maplist(validate_lando_fret_cc(Context), OutFiles, Results),
    sum_list(Results, R).

validate_lando_fret_cc(Context, OutFile, Status) :-
    directory_file_path(OutD, _, OutFile),
    %absolute_file_name(OutFile, OutFilePath),
    file_name_extension(OutBase, 'lus', OutFile),
    string_concat(OutBase, "_result", ResultBase),
    file_name_extension(ResultBase, 'json', ResultFile),
    do_exec(Context, "kind2 lando fret validation",
            [ 'InpFile' = OutFile, 'OutDir' = OutD, 'JSONFile' = ResultFile ],
            [ "pwd",
              "kind2 -json --enable CONTRACTCK --output_dir {OutDir} --timeout 60 {InpFile} > {JSONFile}"
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
show_kind2_result(_, R, Sts, Sts) :-
    print_message(error, unknown_kind2_result(R)).

show_lando_validation_error(Spec, SpecFile, Err) :-
    print_message(error, lando_validation_error(Spec, SpecFile, Err)).

prolog:message(kind2_realizable(Time, Unit)) -->
    [ 'Realizable (~w ~w)' - [ Time, Unit ] ].
prolog:message(unknown_kind2_result(R)) -->
    [ 'Unknown kind2 result: ~w~n' - [ R ] ].
prolog:message(unrecognized_kind2_result_type(OType)) -->
    [ 'Unrecognized result type: ~w~n' - [OType] ].
prolog:message(kind2_log_error(Source, File, Line, Col, Msg)) -->
    [ '~w:~w:~w: ~w error: ~w~n' - [ File, Line, Col, Source, Msg ] ].
prolog:message(invalid_parse(Format, Spec, SpecFile)) -->
    [ 'Validation Error: unable to parse ~w format~n  [~w = ~w]~n' -
      [ Format, SpecFile, Spec ] ].
prolog:message(lando_validation_error(Spec, SpecFile, Err)) -->
    [ 'Validation Error: ~w~n  [~w = ~w]~n' - [ Err, SpecFile, Spec ] ].
prolog :message(number_lando_validation_errors(Spec, SpecFile, Count)) -->
    [ 'Total of ~w validation Errors~n  [~w = ~w]~n' -
      [ Count, SpecFile, Spec ] ].


% ----------------------------------------------------------------------

generate_system_spec(_, InputSpec, R) :-
    with_spec_format_and_file(InputSpec, generate_spec, R).

generate_spec(Spec, "lando", SpecFile, Result) :-
    parse_lando_file(SpecFile, SSL), !,
    findall(R, generate_spec_outputs(Spec, "lando", SSL, R), Results),
    sum_list(Results, Result).
generate_spec(_, _, _, 1).

generate_spec_outputs(Spec, "lando", SSL, Result) :-
    eng:key(system, spec, Spec, generate, OutFile),
    eng:eng(system, spec, Spec, generate, OutFile, format, "json"),
    open(OutFile, write, OutStrm),
    ( write_lando_json(OutStrm, SSL)
    -> Result = 0,
       print_message(information, wrote_file(Spec, OutFile, "json"))
    ; Result = 1,
      print_message(error, did_not_write(Spec, OutFile, "json"))
    ).
generate_spec_outputs(Spec, "lando", SSL, Result) :-
    eng:key(system, spec, Spec, generate, OutFile),
    eng:eng(system, spec, Spec, generate, OutFile, format, "markdown"),
    open(OutFile, write, OutStrm),
    (write_lando_markdown(OutStrm, SSL)
    -> Result = 0,
       print_message(information, wrote_file(Spec, OutFile, "markdown"))
    ; Result = 1,
      print_message(error, did_not_write(Spec, OutFile, "markdown"))
    ).
generate_spec_outputs(Spec, "lando", SSL, Result) :-
    eng:key(system, spec, Spec, generate, OutFile),
    eng:eng(system, spec, Spec, generate, OutFile, format, "fret"),
    open(OutFile, write, OutStrm),
    (write_lando_fret(OutStrm, SSL)
    -> Result = 0,
       print_message(information, wrote_file(Spec, OutFile, "fret"))
    ; Result = 1,
      print_message(error, did_not_write(Spec, OutFile, "fret"))
    ).
generate_spec_outputs(Spec, "lando", SSL, Result) :-
    eng:key(system, spec, Spec, generate, OutDir),
    eng:eng(system, spec, Spec, generate, OutDir, format, "fret_kind2"),
    (write_lando_fret_kind2(OutDir, SSL, OutFiles)
    -> Result = 0,
       wrote_file_messages(Spec, "fret_kind2", OutFiles)
    ; Result = 1,
      print_message(error, did_not_write(Spec, OutDir, "fret_kind2"))
    ).

wrote_file_messages(_, _, []).
wrote_file_messages(Spec, Kind, [OutFile|FS]) :-
    print_message(information, wrote_file(Spec, OutFile, Kind)),
    wrote_file_messages(Spec, Kind, FS).

prolog:message(wrote_file(Spec, OutFile, Kind)) -->
    [ 'Wrote lando spec "~w" to ~w file ~w~n' - [ Spec, Kind, OutFile ]].
prolog:message(did_not_write(_Spec, OutFile, Kind)) -->
    [ 'Unable to write ~w to file ~w due to errors~n' - [ Kind, OutFile ]].
