:- use_module('load.pl').

main :- load_eng, run_eng_cmd
        ; show_help, halt(0).

run_eng_cmd :-
    ingest_engfiles(Context, Refs),
    current_prolog_flag(argv, [Cmd|CmdArgs]),
    (call_eng_cmd(Context, Cmd, CmdArgs, Sts), halt(Sts)
    ; show_help(Cmd), halt(1)).

show_help(NoCmd) :-
    print_message(error, cmd_not_found(NoCmd)),
    show_help,
    halt(1).

show_help :-
    format('Known eng commands:~n'),
    known_command_info(Info),
    writeln(Info).

% get_eng_area :- atom_concat(Cmd, "_focus", CmdFocus), call_

prolog:message(cmd_not_found(Cmd))  -->
    [ 'Sorry, command "~w" does not exist or is implemented erroneously.'-[Cmd] ].
prolog:message(cmd_not_impl(Cmd))  -->
    [ 'Sorry, command "~w" implementation is still TBD.'-[Cmd] ].
