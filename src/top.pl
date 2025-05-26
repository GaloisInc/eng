:- use_module(load).

main :- load_eng, run_eng_cmd
        ; show_help, halt(0).

run_eng_cmd :-
    ingest_user_engfiles(_),
    (current_prolog_flag(argv, [Cmd|CmdArgs]) ; Cmd = "", CmdArgs = []),
    run_each_eng_cmd(Cmd, CmdArgs).

% Several cases handled here:
%
% Result scenarios:
%
% * S1 = all may run successfully
%
% * S2 = some may run successfully, some may error
%
% * S3 = some may run successfully, some may generate warnings (which can be
%        ignored if all other commands are successful).
%
% * E1 = (all) returned an error
%
% * E2 = unrecognized for some, error for others.
%
% * E3 = improper/incomplete/missing specification in eqil files
%
% * E4 = missing sub-command
%
% Command configurations:
%
%  * P: command with no sub-commands: S1, E1, E2, E3
%
%  * S: command with static sub-commands (e.g. doc): S1, S2, S3, E1, E3, E4
%
%  * D: command with dynamic sub-commands (e.g. dev): S1, S2, S3, E1, E2, E3, E4
%
run_each_eng_cmd("", []) :- show_help, !.
run_each_eng_cmd(Cmd, []) :-
    % Handles S:E4
    known_subcommands(Cmd, Sub),
    \+ Sub == [], !,
    known_subcommand_help(Cmd),
    halt(1).
run_each_eng_cmd(Cmd, CmdArgs) :-
    findall(S, run_eng_cmd_each(Cmd, CmdArgs, S), StsAll),
    (StsAll == []
    -> (show_help(Cmd), Sts = 1)
    ; postproc(StsAll, Sts)
    ),
    halt(Sts).

postproc(AllSts, Sts) :- postproc_(AllSts, [], -1, Sts).
postproc_([], [], -1, 0) :- !.  % nothing ran
postproc_([], _, 0, 0) :- !.  % successful, ignore msgs
postproc_([], Msgs, -1, 1) :-
    !,
    list_to_set(Msgs, MsgSet),
    show_msgs(MsgSet).
postproc_([], Msgs, SumSts, Sts) :-
    list_to_set(Msgs, MsgSet),
    show_msgs(MsgSet),
    succ(SumSts, Sts).
postproc_([E|ES], Msgs, SumSts, Sts) :-
    number(E),
    SSts is E + max(0, SumSts),
    postproc_(ES, Msgs, SSts, Sts).
postproc_([E|ES], Msgs, SumSts, Sts) :-
    is_list(E),
    append(E, ES, EES),
    postproc_(EES, Msgs, SumSts, Sts).
postproc_([Msg|ES], Msgs, SumSts, Sts) :-
    postproc_(ES, [Msg|Msgs], SumSts, Sts).

show_msgs([]).
show_msgs([help(M)|Msgs]) :-
    !,
    writeln(M),
    show_msgs(Msgs).
show_msgs([M|Msgs]) :-
    print_message(error, M),
    show_msgs(Msgs).

run_eng_cmd_each(Cmd, CmdArgs, Sts) :-
    call_eng_cmd(Cmd, CmdArgs, Sts).
run_eng_cmd_each(Cmd, CmdArgs, Sts) :-
    ingest_engfiles(Context, Refs),
    call_eng_cmd(Context, Cmd, CmdArgs, Sts),
    erase_refs(Refs).

show_help(NoCmd) :-
    print_message(error, cmd_not_found(NoCmd)),
    show_help,
    halt(1).

show_help :-
    findall(C, ingest_engfiles(C, _), _), % for assert_engfile side-effects
    format('Known eng commands:~n'),
    known_command_info(Info),
    writeln(Info).

prolog:message(cmd_not_found(Cmd))  -->
    [ 'Sorry, command "~w" does not exist or is implemented erroneously.'-[Cmd] ].
prolog:message(cmd_not_impl(Cmd))  -->
    [ 'Sorry, command "~w" implementation is still TBD.'-[Cmd] ].
