:- module(help, [ help_cmd/3, help_focus/1, help_help/1 ]).

:- use_module('../load.pl').
:- use_module('../englib.pl').

help_focus("Help on eng capabilities").

help_cmd(_, [], 1) :-
    format('Please tell me what engineering area you would like help on.~n', []),
    known_command_info(CmdInfo, main_only),
    writeln(CmdInfo).

help_cmd(_, [On|_], 0) :-
    eng_cmd_help(On, HelpInfo),
    format('Help for "~w":~n~n~w~n', [ On, HelpInfo ]),
    % Many eng areas (commands) have specific sub-commands for that area.  If
    % they do, print that information now.  It's OK if they don't.
    (known_subcommand_info(Info, On), \+ Info = [], !, (nl,
                                          format('Subcommands for ~w~n', [On]),
                                          intercalate(Info, "\n", OutStr),
                                          writeln(OutStr))
    ; true
    ).

help_cmd(_, [On|_], 1) :-
    format('You need help on "~w", but I do not have it, sorry!~n', [On]).

help_help(Info) :-
    format(atom(Info), '~w~n~w~n~w~n',
           [ 'I can provide help information for the various engineering',
             'area commands that the eng tool supports.',
             'Run me with the name of the area you would like help on.'
           ]).
