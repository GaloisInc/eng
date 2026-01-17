:- module(help, [ help_cmd/2, help_cmd/3, help_focus/1, help_help/1 ]).

:- use_module('../load.pl').
:- use_module('../englib.pl').

help_focus("Help on eng capabilities").

% General help, regardless of context
help_cmd([], 1) :-
    format('Please tell me what engineering area you would like help on.~n', []),
    known_command_info(CmdInfo, main_only),
    writeln(CmdInfo).


% Command-specific help.
help_cmd([On], 0) :-
    eng_cmd_help(On, HelpInfo),
    !,
    format('Help for "~w":~n~n~w~n', [ On, HelpInfo ]),
    % Many eng areas (commands) have specific sub-commands for that area.  If
    % they do, print that information now.  It's OK if they don't.
    known_subcommand_info(Info, On),
    (Info == []
    -> true
    ; (nl,
       format('Subcommands for ~w~n', [On]),
       intercalate(Info, "\n", OutStr),
       writeln(OutStr))
    ),
    writeln("").
help_cmd([On,'_',OnSub|_], 0) :-
    atom_string(OnSub, OnSubS),
    show_subcmd_focus(On, OnSubS, "_help_internal", full, Infos),
    !,
    maplist(writeln, Infos),
    writeln("").
help_cmd([On,'_'|_], 0) :-
    !,
    known_internal_subcommand_info(On, Info),
    help_cmd_internal(On, Info).
help_cmd([On,OnSub|_], 0) :-
    atom_string(OnSub, OnSubS),
    show_subcmd_focus(On, OnSubS, "_help", full, Infos),
    !,
    maplist(writeln, Infos),
    writeln("").
help_cmd([C|_], 1) :-
    print_message(error, no_help_or_unk_command(C)).
help_cmd(_, 0).

help_cmd(Context, [On|_], 0) :-
    eng_cmd_help(Context, On, Info),
    !,
    context_topdir(Context, TopDir),
    format('[~w] ~`-t~40|~n~n~w~n', [TopDir, Info]).
help_cmd(_, _, 0).


help_cmd_internal(Cmd, []) :-
    format('No internal commands for ~w~n', [Cmd]).
help_cmd_internal(Cmd, Info) :-
    \+ Info = [],
    intercalate(Info, "\n", Internals),
    format('Internal commands for ~w~n~w~n', [ Cmd, Internals ]).


help_help(Info) :-
    format(atom(Info), '~w~n~w~n~w~n',
           [ 'I can provide help information for the various engineering',
             'area commands that the eng tool supports.',
             'Run me with the name of the area you would like help on.'
           ]).

prolog:message(no_help_or_unk_command(C)) -->
    [ 'No help available or unknown command: ~w' - [C]].
