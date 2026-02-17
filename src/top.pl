:- use_module(load).
:- use_module('src/datafmts/eqil').
:- use_module('src/englib').
:- use_module('src/dependencies').

main :- load_eng, run_eng_cmd
        ; show_help, halt(0).

run_eng_cmd :-
    setup_env,
    ingest_user_engfiles(_),
    (current_prolog_flag(argv, [Cmd|CmdArgs]) ; Cmd = "", CmdArgs = []),
    run_each_eng_cmd(Cmd, CmdArgs).

setup_env :-
    current_prolog_flag(tty_control, true), !,
    set_prolog_flag(color_term, true).
setup_env.

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

% Collect and summarize the results of run_eng_cmd_each over a possible set of
% _eng_ directories.
%
% The input AllSts is the list of results of each run_eng_cmd_each, each being:
%
% * a number (0 = success, non-zero = failure)
%
% * an sts(Name, number) value [see below for Name handling]
%
% * a message to be displayed with print_message (error severity)
%
% * a sts(Name, Message) value [see below for Name handling], assumed to be an
%   error (non-zero value).
%
% * a sts(Name, end_msg(Message)) value [see below for Name handlng], with an
%   assumed success (zero) value, and a message to be displayed at the end;
%   sub-eng's with the same message may be combined.
%
% * an unknown(Name, Message) value [see below for Name handling]
%
% * a list to be processed recursively.
%
% Some commands will operate on multiple targets.  For many of these, they will
% return a sts(Name, status) or unknown(Name, status).  The results should be
% segregated by Name and each Name reported separately.  Any unknown(name, msg)
% status values will be ignored if there are other sts(name, X) values present.
%
% The output Sts is 0 on success or non-zero on failure (usually the count of
% failures).  If single_success_ok(Cmd, CmdArgs) is true, then if any Sts is 0
% then all other Sts values are ignored (on a per-Name basis).  Sts is only 0 if
% *all* operations for *all* Names succeeds, or--when single_success_ok is
% true--one operation for every name succeeds.
%
postproc(AllSts, ExitCode) :- postproc_(AllSts, [], -1, ExitCode).
% ...end of status list
postproc_([], [], -1, 0) :- !.  % nothing ran
postproc_([], [], 0, 0) :- !.  % successful and no msgs
postproc_([], Msgs, -1, 1) :-
    !,
    list_to_set(Msgs, MsgSet),
    show_msgs(MsgSet).
postproc_([], Msgs, SumSts, Sts) :-
    list_to_set(Msgs, MsgSet),
    show_msgs(MsgSet),
    succ(SumSts, Sts).
% ... process each status
postproc_([unknown(N, M)|ES], Msgs, SumSts, Sts) :-
    postproc_unk_(N, [M], ES, Msgs, UpdMsgs, RES),
    postproc_(RES, UpdMsgs, SumSts, Sts).
postproc_([sts(N, end_msg(M))|ES], Msgs, SumSts, Sts) :-
    !,
    postproc_sts_(N, ES, RES),
    postproc_(RES, [end_msg(N, M)|Msgs], SumSts, Sts).
postproc_([sts(N, X)|ES], Msgs, SumSts, Sts) :-
    postproc_sts_(N, ES, RES),
    postproc_([X|RES], Msgs, SumSts, Sts).
postproc_([E|ES], Msgs, SumSts, Sts) :-
    number(E),
    SSts is E + max(0, SumSts),
    postproc_(ES, Msgs, SSts, Sts).
postproc_([E|ES], Msgs, SumSts, Sts) :-
    is_list(E),
    append(E, ES, EES),
    postproc_(EES, Msgs, SumSts, Sts).
postproc_([Msg|ES], Msgs, SumSts, Sts) :-
    SSts is 1 + max(0, SumSts),
    postproc_(ES, [Msg|Msgs], SSts, Sts).

% Found unknown(N, Msg), so scan Other for sts(N, X) and if found, remove all
% unknown(N, Msg) from Rem, otherwise convert them all to sts(N, Msg).
postproc_unk_(_, NMsgs, [], Msgs, OutMsgs, []) :-
    % reached end, no sts found, unknowns for N are are msgs now
    append([NMsgs, Msgs], OutMsgs).
postproc_unk_(N, NMsgs, [unknown(N, M)|Other], Msgs, OutMsgs, Rem) :-
    % another unknown for this same name: accumulate their messages
    postproc_unk_(N, [M|NMsgs], Other, Msgs, OutMsgs, Rem).
postproc_unk_(N, _, [sts(N, S)|Other], Msgs, Msgs, [sts(N, S)|Rem]) :-
    % found a sts, drop all unknown by setting UMsgs to sts
    postproc_sts_(N, Other, Rem).
postproc_unk_(N, NMsgs, [L|Other], Msgs, OutMsgs, Rem) :-
    is_list(L),
    append(L, Other, Flat),
    postproc_unk_(N, NMsgs, Flat, Msgs, OutMsgs, Rem).
postproc_unk_(N, NMsgs, [OtherSts|Other], Msgs, OutMsgs, [OtherSts|Rem]) :-
    \+ is_list(OtherSts),
    postproc_unk_(N, NMsgs, Other, Msgs, OutMsgs, Rem).

% Found sts(N, S), so drop all unknown(N, Msg)
postproc_sts_(_, [], []).
postproc_sts_(N, [unknown(N, _)|ES], RES) :- !, postproc_sts_(N, ES, RES).
postproc_sts_(N, [E|ES], [E|RES]) :- postproc_sts_(N, ES, RES).


show_msgs(Msgs) :-
    sort(Msgs, SMsgs),
    show_msgs(SMsgs, Ends),
    show_endmsgs(Ends).
show_msgs([], []).
show_msgs([help(M)|Msgs], GEMs) :-
    !,
    writeln(M),
    show_msgs(Msgs, GEMs).
show_msgs([end_msg(N, M)|Msgs], [end_msg(N, M)|GEMs]) :-
    !,
    show_msgs(Msgs, GEMs).
show_msgs([M|Msgs], GEMs) :-
    print_message(error, M),
    show_msgs(Msgs, GEMs).
show_endmsgs([]).
show_endmsgs(EndMsgs) :-
    format('----Summary----~n'),
    show_endmsgs_(EndMsgs).
show_endmsgs_([]).
show_endmsgs_([end_msg(N, M)|GEMs]) :-
    partition(same_endmsg(M), GEMs, SameMsg, OtherMsg),
    collect_names([end_msg(N,M)|SameMsg], NS),
    print_message(info, endmsgs(M, NS)),
    show_endmsgs_(OtherMsg).
same_endmsg(M, end_msg(_, M)).

collect_names([], []).
collect_names([end_msg(N, _)|GEMs], [N|NS]) :- collect_names(GEMs, NS).

run_eng_cmd_each(Cmd, CmdArgs, Sts) :-
    % Runs all commands that do not take a Context.  This is essentially invoked
    % once, then the Context-passing version below is tried.  Commands that do
    % not take a Context do not wildcard/don't-care the Context argument because
    % they would then be invoked multiple times for each Context.
    call_eng_cmd(Cmd, CmdArgs, Sts).
run_eng_cmd_each(Cmd, CmdArgs, Sts) :-
    findall((Context, Parsed), ingest_engfiles(Context, Parsed), AllConPars),
    (ordered(AllConPars, SortedConPars), ! ; SortedConPars = AllConPars),
    length(SortedConPars, NumCons),
    run_eng_cmd_each_(Cmd, CmdArgs, NumCons, SortedConPars, Sts).
run_eng_cmd_each_(_, _, _, [], []).
run_eng_cmd_each_(Cmd, CmdArgs, N, [(Context, Parsed)|CPS], [S|SS]) :-
    (N = 1, ! ;
     print_message(informational, run_cmd_in_context(Cmd, CmdArgs, Context))
    ),
    assert_eqil(Parsed, Refs),
    call_eng_cmd(Context, Cmd, CmdArgs, S),
    erase_refs(Refs),
    run_eng_cmd_each_(Cmd, CmdArgs, N, CPS, SS).


% Use the sort_subprojects/3 in the dependency module to sort projects into the
% deps-first order.  The vctl module uses the vctl.subproject EQIL input to do
% this; here, we could assert the Parsed EQIL to do this (all of them?  just the
% top level?), but we can effectively just sort based on the projects we've
% discovered.  The difference is that vctl.subproject may have more entries, but
% the EQIL (just) has the local entries; here we are only selecting the order of
% the local entries to operate on, so it's fine to not know about the non-local
% entries.
ordered(ConPars, SortedConPars) :-
    % First, find and remove the top-level project Context.  It will always come
    % last after all the subprojects.  In addition, this needs to be able to
    % locate all the subprojects relative to that top-level.
    partition(is_top_conpar, ConPars, Top, Sub),
    !,
    ordered_(Top, Sub, SortedConPars),
    !.
ordered_([Top], Sub, SortedConPars) :-
    top_conpar_context(Top, TopContext),
    % Convert remaining into somethig sort_subprojects can use
    maplist(get_dep_checker(TopContext), Sub, SubAndCheckers),
    EntryInfo = entry{ entMyDepName: getname, entDepChecker: getchecker },
    % Sort into dependency or alphanumeric order
    sort_subprojects(SubAndCheckers, SortedSubs, EntryInfo),
    % Now restore the original entry values and append the top-most entry
    restore_and_append(SortedSubs, Top, SortedConPars).
ordered_(O, Sub, Sub) :-
    print_message(error, invalid_top_conpar(O)).

is_top_conpar(ConPar) :- top_conpar_context(ConPar, _).
top_conpar_context((Context, _), Context) :- context_reltip(Context, '<here>').

get_dep_checker(Context, (SubContext, SP),
                pdi(SubContext, SP, MyName, DepChecker)) :-
    context_reltip(SubContext, ConSubDir),
    get_dependency_checker(Context, ConSubDir, MyName, DepChecker).

getname(pdi(_, _, Name, _), Name).
getchecker(pdi(_, _, _, Checker), Checker).

restore_and_append([], L, [L]).
restore_and_append([pdi(SC, SP, _, _)|PS], L, [(SC, SP)|SPS]) :-
    restore_and_append(PS, L, SPS).


show_help(NoCmd) :-
    print_message(error, cmd_not_found(NoCmd)),
    show_help,
    halt(1).

show_help :-
    findall(Refs, (ingest_engfiles(_, P), assert_eqil(P, Refs)), _AllRefs),
    format('Known eng commands:~n'),
    known_command_info(Info),
    writeln(Info).

prolog:message(endmsgs(Msg, NameList)) -->
    {
        Width = 80,
        length(NameList, NumNames),
        format(atom(Hdr), '~w/~w: ', [Msg, NumNames])
    },
    [ Hdr ],
    {
        clone_string_repchars(Hdr, ' ', Indent),
        pretty_intercalate(NameList, ", ", Width, Indent, Names)
    },
    [ Names ].
prolog:message(run_cmd_in_context(Cmd, [], Context)) -->
    { context_topdir(Context, TopDir) },
    % NABLA 2207 ∇
    [ '∇∇∇∇∇∇∇∇ ~w ~w :' - [ TopDir, Cmd ] ].
prolog:message(run_cmd_in_context(Cmd, [A|_], Context)) -->
    { context_topdir(Context, TopDir) },
    [ '∇∇∇∇∇∇∇∇ ~w ~w ~w :' - [ TopDir, Cmd, A ] ].

prolog:message(cmd_not_found(Cmd))  -->
    [ 'Sorry, command "~w" does not exist or is implemented erroneously.'-[Cmd] ].
prolog:message(cmd_not_impl(Cmd))  -->
    [ 'Sorry, command "~w" implementation is still TBD.'-[Cmd] ].
