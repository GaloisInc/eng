:- module(run, [ run_cmd/3, run_focus/1, run_help/1, run_help/2 ]).

:- use_module(library(strings)).
:- use_module('../englib').
:- use_module(exec_subcmds).

run_focus("Execution Engineering").

run_help(Info) :-
    engfile_dir(EngDirV),
    exec_subcmd_help("run", ExecHelpI),
    [EngDir, ExecHelp] = [EngDirV, ExecHelpI],
    Info = {|string(EngDir, ExecHelp)||
| Perform an EXECUTION engineering task.
|
| Execution tasks typically consist of running one or more user-defined
| operations that represent the results of the development engineering
| activities.
|
| Execution information is specified in one or more .eng files
| in the {EngDir} directory of the project.  These files are typically in
| EQIL format (see the EQIL design document), and have the following structure
| (uppercase is user-specified input):
|
| {ExecHelp}
|}.

run_help(SubCmd, Help) :- eng:key(run, subcmd, SubCmd, Help).

run_cmd(Context, [Cmd|Args], sts(Cmd, Sts)) :-
    exec_subcmd_do(Context, run, Cmd, Args, Sts).
run_cmd(Context, [Cmd|_], unknown(Cmd, invalid_subcmd(run, Context, Cmd))).
