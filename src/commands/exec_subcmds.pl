:- module(exec_subcmds, [ exec_subcmd_help/2,
                          exec_subcmd_do/5,
                          exec_spec_help/2,
                          exec_from_spec_at/4,
                          exec_with_shell_wrapper/4,
                          do_exec/7,
                          set_env_vars/2,
                          prep_args/3
                        ]).

:- use_module(library(lists)).
:- use_module(library(strings)).
:- use_module(library(yall)).
:- use_module(library(dcg/basics)).
:- use_module('../englib').

% This module does not define a command directly, but provides definitions to
% support the use of the eng configuration files to define sub-commands.  These
% definitions can be used with other commands that wish to introduce this
% extensible functionality.

exec_spec_help(Indent, Info) :-
    OrigInfo = {|string||
| exec = SHELL COMMAND(S) TO RUN WITH Args
| exec = ANOTHER SHELL COMMAND
| in dir = DIRECTORY
| nix shell pkgs = ...
| env vars =
|   VARNAME = VARVALUE
|   ...
|},
    split_string(OrigInfo, "\n", "", Lines),
    string_rpad("\n", " ", Indent, Pfx),
    intercalate(Lines, Pfx, Info).


exec_subcmd_help(Cmd, Info) :-
    exec_spec_help(12, EInfo),
    [CmdHelp, ExecSpecHelp] = [Cmd, EInfo],
    Info = {|string(CmdHelp, ExecSpecHelp)||
| Sub-commands which run an external executable can be defined:
|
|     {CmdHelp} =
|       subcmd =
|         CMDNAME =
|           DESCRIPTION =
|             needs = CMDNAME(s) TO RUN BEFORE THIS ONE
|             {ExecSpecHelp}
|
| The "exec" value can contain Args in curly braces: this will be replaced with
| any additional arguments specified on the eng command-line.  The first word
| of the each exec specification is replaced by any 'exec bin' value, allowing
| high-level use of any launcher (e.g. "nix run").  Note that an "exec" with
| multiple lines will only have the first such word replaced (and the entire
| block is executed in a single shell operation); multiple "exec" keys can
| specify multiple lines that will be executed in sequence with the first word
| of each allowing an 'exec bin' substitution:
|
|     exec bin =
|       BIN1 = SUBST1
|       ...
|     exec env vars =
|       VARNAME = VARVALUE
|       ...
|
| All entries are optional.
|
| In addition to the above substitution (and alternatively), the optional
| "nix shell pkgs" may be specified, which will cause the "exec" to be
| performed in a nix (nixos.org) shell with the specified nix packages
| available; if the "nix" command is not available, this line is ignored.
|
| If provided, environment variables will be set and the current directory will
| be set to "in dir" before the "Exec" is run. The "exec", "in dir", and
| "env vars" values may contain EngDir and/or TopDir in curly braces: these will
| be replaced with the directory containing the eng specifications and the
| directory containing the EngDir, respectively.  Any "exec env vars" will be
| set for *all* executed commands.  The "exec bin" and "exec env vars" settings
| are commonly found in the home configuration directory configuration for the
| user (e.g. /home/USER/.config/eng/___.eng).
|}.

exec_subcmd_do(Context, Cmd, SubCmd, Args, Sts) :-
    eng:key(Cmd, subcmd, SubCmd), !,
    exec_subcmd_ready(Context, Cmd, SubCmd, Args, Sts).

exec_subcmd_ready(Context, Cmd, SubCmd, Args, Sts) :-
    exec_subcmd_each(Context, Cmd, SubCmd, Args, Sts).

exec_subcmd_each(Context, Cmd, SubCmd, Args, AllSts) :-
    % Each SubCmd may have multiple instances delineated by the Descr.  The Descr
    % provides a summary for help information, and helps to keep the key/values
    % for different parts of the SubCmd isolated (SubCmds are compositional).
    findall(D, eng:key(Cmd, subcmd, SubCmd, D), Descrs),
    maplist(exec_subcmd_descr(Context, Cmd, SubCmd, Args), Descrs, AllSts).


exec_subcmd_descr(Context, Cmd, SubCmd, Args, Descr, sts(SubCmd, Sts)) :-
    eng:eng(Cmd, subcmd, SubCmd, Descr, needs, PreCmds),
    split_string(PreCmds, " \n\t", " \t\n", PreCmdList),
    member(PreCmd, PreCmdList),
    atom_string(PCmd, PreCmd),
    exec_subcmd_do(Context, Cmd, PCmd, Args, Sts),
    %% If the needed command was NOT successful, stop.  If it was successful,
    %% this will fail and backtrack, either to the next needed command
    %% (member(...) above) or to the next clause which runs the originally
    %% requested command.
    \+ is_success(Sts),
    !.
exec_subcmd_descr(Context, Cmd, SubCmd, Args, Descr, sts(SubCmd, Sts)) :-
    subcmd_args_argmap(Args, ArgMap),
    exec_from_spec_at(Context, ArgMap, [Cmd, subcmd, SubCmd, Descr], Sts).

subcmd_args_argmap(Args, [ 'Args' = ArgStr ]) :-
    maplist([E,O]>>format(atom(O), "\"~w\"", [E]), Args, QuotedArgs),
    foldl([S,A,O]>>(string_concat(A, " ", AS),
                    string_concat(AS, S, O)), QuotedArgs, "", ArgStr).

%% ----------------------------------------------------------------------

%% exec_from_spec_at is a general function that performs a do_exec based on the
%% standard exec configuration found at the specified root key.  If there is no
%% exec configuration, this will fail.

exec_from_spec_at(Context, ArgMap, RootPath, Result) :-
    append(RootPath, [exec], ExecPath),
    findall(E, list_call(eng:eng, ExecPath, E), ES),
    (ES == []
    -> fail
    ; exec_from_spec_at(Context, ArgMap, RootPath, ES, Result)
    ).

exec_from_spec_at(Context, ArgMap, RootPath, ExecCmds, Result) :-
    append(RootPath, ['env vars'], EnvPath),
    exec_env_vars(EnvPath, VS),
    exec_from_spec_at_dir(RootPath, ExecDir),
    maplist(atom_string, RootPath, SP),
    intercalate(SP, ".", Ref),
    exec_with_shell_wrapper(Context, RootPath, ExecCmds, ActualCmds),
    do_exec(Context, Ref, ArgMap, ActualCmds, VS, ExecDir, Result).

exec_env_vars(EnvPath, VS) :-
    findall((N,V), (list_call(eng:key, EnvPath, N),
                    append(EnvPath, [N], NP),
                    list_call(eng:eng, NP, VR),
                    string_trim(VR, V)
                   ), VS).

:- dynamic eng:use_dir/3.

exec_from_spec_at_dir(RootPath, ExecDir) :-
    append(RootPath, ['in dir'], DirPath),
    list_call(eng:eng, DirPath, ExecDir),
    !.
exec_from_spec_at_dir([Cmd,SubCmd|_], ExecDir) :-
    list_call(eng:use_dir, [Cmd,SubCmd], ExecDir),
    !.
exec_from_spec_at_dir(_, curdir).

exec_with_shell_wrapper(Context, RootPath, ExecCmds, UpdExecCmds) :-
    append(RootPath, ['nix shell pkgs'], NixShellKey),
    findall(E, list_call(eng:eng, NixShellKey, E), ES),
    (ES == [] -> UpdExecCmds = ExecCmds
    ; in_nix_shell_with(Context, ES, ExecCmds, UpdExecCmds)
    ).

in_nix_shell_with(Context, Pkgs, ExecCmds, UpdExecCmds) :-
    exe_lookup(nix, Path),
    dir_runproc(Context, "nix tool check", Path, ["--version"], curdir, _Out),
    !,
    maplist([I,O]>>split_string(I, "\n \t", "", O), Pkgs, PkgSplit),
    append(PkgSplit, AllPkgs),
    maplist(string_trim, AllPkgs, JustPkgs),
    intercalate(JustPkgs, " ", PkgArgs),
    maplist([E,O]>>format(atom(O), "~w shell ~w --command bash -c '~w'",
                          [ Path, PkgArgs, E ]), ExecCmds, UpdExecCmds).
in_nix_shell_with(_, _, ExecCmds, ExecCmds). % no nix tool available


exe_lookup(ExeName, ExePath) :-
    absolute_file_name(ExeName, Path, [access(execute), file_errors(fail)]),
    !,
    (read_link(Path, _, ExePath) ; ExePath = Path).
exe_lookup(ExeName, ExePath) :-
    absolute_file_name(path(ExeName), Path, [access(execute), file_errors(fail)]),
    !,
    (read_link(Path, _, ExePath) ; ExePath = Path).
exe_lookup(ExeName, _) :-
    print_message(error, exe_not_found(ExeName)), fail.

prolog:message(exe_not_found(ExeName)) -->
    [ 'Executable not found on PATH: ~w' - [ExeName] ].

%% ----------------------------------------------------------------------

%% do_exec is a general function to execute a list of commands or a single
%% command string, setting the specified environment variables, possibly in a
%% specified directory, and substituting various arguments into the executed
%% command.  It can also substitute EngDir and TopDir in the environment variable
%% values and target directory).  The result is returned in the last argument: 0
%% on success or non-zero on failure.

do_exec(_, _Ref, _ArgMap, [], _EnvVars, _InDir, 0).
do_exec(Context, Ref, ArgMap, [C|CS], EnvVars, InDir, Sts) :-
    do_exec_single(Context, Ref, ArgMap, C, EnvVars, InDir, ThisSts),
    ( ThisSts = 0, !, do_exec(Context, Ref, ArgMap, CS, EnvVars, InDir, Sts)
    ; Sts = ThisSts,
      (CS == [], !
      ; length(CS, CSL),
        print_message(warning, abort_exec_on_failure(Ref, CSL))
      )
    ).
do_exec(Context, Ref, ArgMap, C, EnvVars, InDir, Sts) :-
    string(C),
    do_exec_single(Context, Ref, ArgMap, C, EnvVars, InDir, Sts).

% Run the C, which is a list of executable and args, capturing the stdout.  Only
% completes if the command exits successfully.
do_exec(Context, Ref, ArgMap, capture(C), EnvVars, InDir, Sts) :-
    do_exec_single(Context, Ref, ArgMap, capture(C), EnvVars, InDir, Sts).

do_exec_single(Context, Ref, ArgMap, capture([Cmd|Args]), EnvVars, InDir, StdOut) :-
    !,
    set_env_vars(Context, EnvVars),
    subst_exec(Cmd, E),
    maplist(prep_args(ArgMap), Args, EArgs),
    dir_runproc(Context, Ref, E, EArgs, InDir, StdOut).
do_exec_single(Context, Ref, ArgMap, [Cmd|Args], EnvVars, InDir, Sts) :-
    !,
    set_env_vars(Context, EnvVars),
    subst_exec(Cmd, E),
    maplist(prep_args(ArgMap), Args, EArgs),
    (InDir == curdir -> PCtl = []; PCtl = [cwd(InDir)]),
    intercalate([Cmd|Args], " ", FullExec),
    print_message(informational, running_exec(FullExec)),
    exe_lookup(E, ExePath),
    catch((process_create(ExePath, EArgs, PCtl), Sts = 0),
          _,
          (print_message(error, exec_failure(Ref, FullExec, 1)), Sts = 1)
         ).
do_exec_single(Context, Ref, ArgMap, ShellCmd, EnvVars, InDir, Sts) :-
    set_env_vars(Context, EnvVars),
    subst_exec(ShellCmd, ExecCmd),
    prep_args(ArgMap, ExecCmd, FullExec),
    print_message(informational, running_exec(FullExec)),
    dir_shell(Context, Ref, FullExec, InDir, ThisSts),
    ( ThisSts = 0, !, Sts = ThisSts
    ; Sts = ThisSts,
      print_message(error, exec_failure(Ref, ShellCmd, Sts))
    ).


%% ----------------------------------------------------------------------

subst_exec(Cmd, Actual) :-
    split_string(Cmd, "\n", "", Cmds), length(Cmds, NCmds), NCmds > 1, !,
    maplist(subst_exec, Cmds, SCmds),
    intercalate(SCmds, "\n", Actual).
subst_exec(Cmd, Actual) :-
    atom_string(Cmd,CmdS),
    string_codes(CmdS, Chars),
    phrase(exec_bin(ActualExec), Chars, Remaining), !,
    atom_string(ActualExec, ExecStr),
    string_concat(ExecStr, " ", E1),
    string_codes(RemString, Remaining),
    string_concat(E1, RemString, Actual).
subst_exec(Cmd, Cmd).

exec_bin(E) --> { eng:eng('exec bin', Cmd, E),
                  atom_string(Cmd,CmdS),
                  string_codes(CmdS, CmdCodes)
                },
                CmdCodes, endword.
endword --> " ".
endword --> eos.

prep_args(ArgMap, TE, TECmd) :-
    catch(interpolate_string(TE, TECmd, ArgMap, []),
          _Err, % error(existence_error(procedure, CmdHPred/2), context(_,_))

          %% Here, the interpolate_string has failed.  This could be because the
          %% wrong X was used in an {X} substitution, or it could just be that
          %% the command string contains a {X} that should be passed-through as
          %% is.  Options are to (1) fail, which breaks the second case, or (2)
          %% just use the input string.  The preference here is (2) to support
          %% the second case, but note that it's all or nothing: if the input
          %% string contains "foo {X} as {Y}" and {X} is supposed to be
          %% substituted but {Y} is supposed to be passed through, the failure to
          %% substitute {Y} will return without {X} substituted either.
           TECmd = TE).


%% ----------------------------------------------------------------------

set_env_vars(Context, EnvVars) :-
    % Apply global env vars before command-specific env vars so that the latter
    % can override the former.
    global_set_env_vars(Context),
    set_env_vars_(Context, EnvVars).
global_set_env_vars(Context) :-
    findall((N, V), eng:eng('exec env vars', N, V), GlobalEnvVars),
    set_env_vars_(Context, GlobalEnvVars).
set_env_vars_(_, []).
set_env_vars_(Context, [(VarName, VarVal)|EnvVars]) :-
    context_topdir(Context, TopDir),
    context_engdir(Context, EngDir),
    prep_args(['EngDir' = EngDir, 'TopDir' = TopDir ], VarVal, SubstVarVal),
    string_trim(SubstVarVal, SetVarVal),
    setenv(VarName, SetVarVal),
    set_env_vars_(Context, EnvVars).

%% ----------------------------------------------------------------------

dir_shell(_, _, FullExec, curdir, ExecSts) :- shell(FullExec, ExecSts).
dir_shell(Context, Ref, FullExec, InDir, ExecSts) :-
    context_topdir(Context, TopDir),
    context_engdir(Context, EngDir),
    prep_args(['EngDir' = EngDir, 'TopDir' = TopDir ], InDir, SubstDir),
    string_trim(SubstDir, TgtDir),
    (exists_directory(TgtDir), !,
     working_directory(OldDir, TgtDir),
     shell(FullExec, ExecSts),
     working_directory(_, OldDir)
    ; print_message(error, invalid_directory(Ref, TgtDir)),
      ExecSts = 1
    ).

%% ----------------------------------------------------------------------

dir_runproc(_, _, Exe, Args, curdir, StdOut) :-
    !,
    exe_lookup(Exe, ExePath),
    debug(exec, run_exec(Exe, ExePath, Args), []),
    setup_call_cleanup(
        process_create(ExePath, Args, [stdout(pipe(Out)),
                                       process(PID)]),
        read_lines(Out, StdOut),
        (safe_close(Out), process_wait(PID, Sts))),
    % according to the docs, process_create is not supposed to continue if the
    % process failed, but evidence shows otherwise, so here we use process_wait
    % to get the final status and below we ensure failure does not progress.
    !,
    Sts = exit(0).
dir_runproc(Context, _Ref, Exe, Args, InDir, StdOut) :-
    context_topdir(Context, TopDir),
    context_engdir(Context, EngDir),
    prep_args(['EngDir' = EngDir, 'TopDir' = TopDir ], InDir, SubstDir),
    string_trim(SubstDir, TgtDir),
    exe_lookup(Exe, ExePath),
    debug(exec, run_exec_in_dir(Exe, ExePath, Args, TgtDir), []),
    setup_call_cleanup(
        process_create(ExePath, Args, [stdout(pipe(Out)), cwd(TgtDir),
                                       process(PID)]),
        read_lines(Out, StdOut),
        (safe_close(Out), process_wait(PID, Sts))),
    % according to the docs, process_create is not supposed to continue if the
    % process failed, but evidence shows otherwise, so here we use process_wait
    % to get the final status and below we ensure failure does not progress.
    !,
    Sts = exit(0).


% Used because sometimes close itself can throw an exception
safe_close(Stream) :- catch(close(Stream), _, true).

read_lines(Out, Lines) :-
    read_line_to_codes(Out, Line1),
    read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.
read_lines(Codes, Out, [Line|Lines]) :-
    atom_codes(Line, Codes),
    read_line_to_codes(Out, Line2),
    read_lines(Line2, Out, Lines).

%% ----------------------------------------------------------------------

prolog:message(run_exec(Exe, ExePath, Args)) -->
    [ 'Run ~w as ~w with ~w~n' - [Exe, ExePath, Args] ].
prolog:message(run_exec_in_dir(Exe, ExePath, Args, Dir)) -->
    [ 'Run ~w (~w) with ~w in ~w~n' - [Exe, ExePath, Args, Dir] ].
prolog:message(invalid_directory(Ref, InDir)) -->
    [ 'Requested exec target directory ~w does not exist when running ~w' -
      [ InDir, Ref ] ].
prolog:message(running_exec_subcmd(Cmd, SubCmd, Descr)) -->
    [ 'Running "~w" sub-command "~w" (~w)' - [ Cmd, SubCmd, Descr ] ].
prolog:message(running_exec(Exec)) -->
    { string_concat("$ ", Exec, O) },
    [ '~w' - [ O ] ].
prolog:message(exec_failure(Ref, C, Sts)) -->
    [ 'ERROR ~w executing ~w: ~w' - [ Sts, Ref, C ] ].
prolog:message(abort_exec_on_failure(Ref, SkipCnt)) -->
    [ 'ERROR skipping ~w commands due to previous failures for ~w'
      - [ SkipCnt, Ref ] ].
