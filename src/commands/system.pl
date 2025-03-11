:- module(system_eng, [ system_cmd/3, system_focus/1,
                        system_help/1, system_help/2 ]).
% n.b. the module name cannot be "system" because that is reserved in SWI Prolog.

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(strings)).
:- use_module('../englib').
:- use_module('../load').
:- use_module('../datafmts/lando').
:- use_module('../lando_tool').

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
system_cmd(_, ['validate', 'ALL'], Result) :-
    process_system_specs(validate_system_spec, Result).
system_cmd(_, ['validate'|Specs], Result) :-
    process_system_specs(validate_system_spec, Specs, Result).

system_cmd(_, ['gen'], 1) :- print_message(error, specify_ssl_id).
system_cmd(_, ['gen', 'ALL'], Result) :-
    process_system_specs(generate_system_spec, Result).
system_cmd(_, ['gen'|Specs], Result) :-
    process_system_specs(generate_system_spec, Specs, Result).

system_cmd(_, [Cmd|_], 1) :-
    print_message(error, invalid_system_subcmd(Cmd)).

% ----------------------------------------------------------------------

process_system_specs(Op, Result) :-
    (setof(S, eng:key(system, spec, S), Specs); Specs = []),
    maplist(Op, Specs, Results),
    sum_list(Results, Result).

process_system_specs(Op, Specs, Result) :-
    findall(R, (member(Spec, Specs), call(Op, Spec, R)), Results),
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
    format('  [~w] ~w: ~w~n', [ Format, Spec, Name ]),
    fail. % backtrack and try the next one

% ----------------------------------------------------------------------

validate_system_spec(InputSpec, R) :-
    with_spec_format_and_file(InputSpec, validate_spec, R).

validate_spec(Spec, "lando", SpecFile, 0) :-
    parse_lando_file(SpecFile, _SSL), !,
    format("TBD: validation of lando spec: ~w~n", [ Spec ]).
validate_spec(_, _, _, 1).

% ----------------------------------------------------------------------

generate_system_spec(InputSpec, R) :-
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

prolog:message(wrote_file(Spec, OutFile, Kind)) -->
    [ 'Wrote lando spec "~w" to ~w file ~w~n' - [ Spec, Kind, OutFile ]].
prolog:message(did_not_write(_Spec, OutFile, Kind)) -->
    [ 'Unable to write ~w to file ~w due to errors~n' - [ Kind, OutFile ]].
