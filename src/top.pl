:- use_module('load.pl').

main :- load_eng, run_eng_cmd
        ; show_help, halt(0).

run_eng_cmd :-
    ingest_user_engfiles(_),
    (current_prolog_flag(argv, [Cmd|CmdArgs]) ; Cmd = "", CmdArgs = []),
    findall(S, run_eng_cmd_each(Cmd, CmdArgs, S), StsAll),
    (StsAll == []
    -> (current_prolog_flag(argv, [Cmd|_]), show_help(Cmd), Sts = 1)
    ; postproc(StsAll, Sts)
    ),
    halt(Sts).

postproc(AllSts, Sts) :- postproc_(AllSts, [], -1, Sts).
postproc_([], [], -1, 0) :- !.  % nothing ran
postproc_([], _, 0, 0) :- !.  % successful, ignore msgs
postproc_([], Msgs, -1, 1) :- !, show_msgs(Msgs).
postproc_([], Msgs, SumSts, Sts) :-
    show_msgs(Msgs),
    succ(SumSts, Sts).
postproc_([E|ES], Msgs, SumSts, Sts) :-
    number(E),
    SSts is E + max(0, SumSts),
    postproc_(ES, Msgs, SSts, Sts).
postproc_([Msg|ES], Msgs, SumSts, Sts) :-
    postproc_(ES, [Msg|Msgs], SumSts, Sts).

show_msgs([]).
show_msgs([M|Msgs]) :-
    print_message(error, M),
    show_msgs(Msgs).

run_eng_cmd_each(Cmd, CmdArgs, Sts) :-
    ingest_engfiles(Context, Refs),
    call_eng_cmd(Context, Cmd, CmdArgs, Sts),
    erase_refs(Refs).

show_help(NoCmd) :-
    print_message(error, cmd_not_found(NoCmd)),
    show_help,
    halt(1).

show_help :-
    findall(C, ingest_engfiles(C, _), CS), % for assert_engfile side-effects
    format('Known eng commands:~n'),
    known_command_info(Info),
    writeln(Info).

prolog:message(cmd_not_found(Cmd))  -->
    [ 'Sorry, command "~w" does not exist or is implemented erroneously.'-[Cmd] ].
prolog:message(cmd_not_impl(Cmd))  -->
    [ 'Sorry, command "~w" implementation is still TBD.'-[Cmd] ].
