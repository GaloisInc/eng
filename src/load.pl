:- module(load, [ load_eng/0,
                  known_commands/1,
                  known_command_focus/2,
                  known_command_info/1,
                  known_command_info/2,
                  known_subcommands/2,
                  known_subcommand_help/1,
                  known_subcommand_info/2,
                  call_eng_cmd/4,
                  call_eng_cmd/3,
                  eng_cmd_help/2,
                  eng_cmd_help/3,
                  engfile_dir/1,
                  ingest_user_engfiles/1,
                  ingest_engfiles/2,
                  erase_refs/1
                ]).

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(englib).

% Note: Each time a new primary command (engineering focus) is added, add it to
% load_eng and known_commands.

load_eng.   % This used to have the use_module statements below as the body, but
            % this prevented them from being placed in the saved state for
            % distribution, so those are now top-level and this does nothing.
:- use_module('src/commands/dev').
:- use_module('src/commands/doc').
:- use_module('src/commands/help').
:- use_module('src/commands/run').
:- use_module('src/commands/system').
:- use_module('src/commands/exec_subcmds').
:- use_module('src/commands/versionctl').
:- use_module('src/datafmts/eqil').

known_commands([
                      "dev",
                      "doc",
                      "help",
                      "run",
                      "system",
                      "vctl" ]).

known_command_focus(Cmd, cmdfocus(Cmd, CmdFocus)) :-
    string_concat(Cmd, "_focus", CmdF),
    atom_string(CmdFPred, CmdF),
    ( current_predicate(CmdFPred, _), !, call(CmdFPred, CmdFocus)
    ; %% print_message(error, cmd_not_impl(Cmd)),
      CmdFocus = ""
    ).

known_command_info(Info) :-
    known_commands(Cmds),
    sort(Cmds, SortedCmds),
    maplist(known_command_focus, SortedCmds, CLS),
    maplist(show_cmd_focus(with_subcommands), CLS, ILS),
    append(ILS, ILSS),
    intercalate(ILSS, "\n", Info).

known_command_info(Info, main_only) :-
    known_commands(Cmds),
    sort(Cmds, SortedCmds),
    maplist(known_command_focus, SortedCmds, CLS),
    maplist(show_cmd_focus, CLS, ILS),
    intercalate(ILS, "\n", Info).

show_cmd_focus(cmdfocus(Cmd, CmdFocus), OutStr) :-
    format(atom(OutStr), '  ~w ~`-t ~w~72|', [ Cmd, CmdFocus ]).

show_cmd_focus(with_subcommands, cmdfocus(Cmd, CmdFocus), [OutCmd|OutSub]) :-
    format(atom(OutCmd), '  ~w ~`-t ~w~72|', [ Cmd, CmdFocus ]),
    %% format(atom(OutCmd), '  ~w~`-t~14+ ~w~72|', [ Cmd, CmdFocus ]),
    % n.b. format outputs an atom, not a string...
    known_subcommand_info(OutSub, Cmd).

known_subcommand_info(Info, Cmd) :-
    findall(I, show_subcmd_focus(Cmd, _, I), SInfo),
    append(SInfo, SI),
    list_to_set(SI, Lines),
    sort(Lines, Info).

known_subcommands(Cmd, SubCmds) :-
    string_concat(Cmd, "_help", CmdH),
    atom_string(CmdHPred, CmdH),
    % Get the list of subcommands, and allow no failures either in the CmdHPred
    % or if there are no subcommands (the .eng file doesn't exist, is badly
    % formatted, or doesn't provide subcommands).
    (catch(setof(S, H^call(CmdHPred, S, H), SubCmds),
           _Err, % error(existence_error(procedure, CmdHPred/2), context(_,_))
           SubCmds = [])
    ; SubCmds = []
    ).

show_subcmd_focus(Cmd, SubCmd, [OutStr]) :-
    get_subcmd_focus(Cmd, SubCmd, OutStr).
show_subcmd_focus(Cmd, SubCmd, OutStr) :-
    ingest_engfiles(_Context, Refs, silent),
    findall(O, get_subcmd_focus(Cmd, SubCmd, O), OutStr),
    erase_refs(Refs).
get_subcmd_focus(Cmd, SubCmd, OutStr) :-
    atom_string(Cmd, CmdS),
    string_concat(CmdS, "_help", CmdH),
    atom_string(CmdHPred, CmdH),
    catch(call(CmdHPred, SubCmd, H), _Err, fail),
    (is_list(H), H = [CmdHelp|_]
    ; \+ is_list(H), CmdHelp = H
    ),
    format(atom(OutStr), '     ~w ~`.t~18+ ~w~72|', [ SubCmd, CmdHelp ]).

call_eng_cmd(Cmd, [], Msg) :-
    % If Cmd was not given arguments and this is a command that expects a
    % sub-command, provide the user with help on the available sub-commands.
    known_subcommands(Cmd, Sub),
    \+ Sub == [], !,
    known_subcommand_help(Cmd, Msg).

call_eng_cmd(Cmd, CmdArgs, Sts) :-
    string_concat(Cmd, "_cmd", CmdOp),
    atom_string(CmdPred, CmdOp),
    ( current_predicate(CmdPred, G), head_name_arity(G, CmdPred, 2), !
    ; %% print_message(error, cmd_not_impl(Cmd)),
      fail),
    call(CmdPred, CmdArgs, Sts).

call_eng_cmd(_, Cmd, [], Msg) :-
    % If Cmd was not given arguments and this is a command that expects a
    % sub-command, provide the user with help on the available sub-commands.
    known_subcommands(Cmd, Sub),
    \+ Sub == [], !,
    known_subcommand_help(Cmd, Msg).

call_eng_cmd(Context, Cmd, CmdArgs, Sts) :-
    string_concat(Cmd, "_cmd", CmdOp),
    atom_string(CmdPred, CmdOp),
    ( current_predicate(CmdPred, G), head_name_arity(G, CmdPred, 3), !
    ; %% print_message(error, cmd_not_impl(Cmd)),
      fail),
    call(CmdPred, Context, CmdArgs, Sts).

known_subcommand_help(Cmd, help(Help)) :-
    format('Please specify one of the ~w engineering sub-commands to perform:~n',
          [ Cmd ]),
    known_subcommand_info(Info, Cmd), !,
    intercalate(Info, "\n", OutStr),
    format(atom(Help), "~w", [OutStr]).
known_subcommand_help(Cmd) :-
    known_subcommand_help(Cmd, help(OutStr)),
    writeln(OutStr).

eng_cmd_help(Cmd, HelpInfo) :-
    string_concat(Cmd, "_help", S),
    atom_string(CmdHelp, S),
    current_predicate(CmdHelp, _),
    call(CmdHelp, HelpInfo).

eng_cmd_help(Context, Cmd, HelpInfo) :-
    string_concat(Cmd, "_help", S),
    atom_string(CmdHelp, S),
    catch(call(CmdHelp, Context, HelpInfo), _Err, fail).

engfile_dir("_eng_").

% Entrypoint: assume "Dir" is relative, may have multiple elements, and may
% be relative to the current working directory: convert it to absolute and
% start the search from that parent.
%
% This must be called from somewhere within a tree; not finding an engfile
% anywhere above the current point is an error.
find_engfile_dir(Dir, EngDir) :-
    find_engfile_tree(Dir, Tree),
    select_engfile_dir(Tree, EngDir).

select_engfile_dir(engnode(_,SS), EngDir) :-
    member(Ent, SS),
    select_engfile_dir(Ent, EngDir).
select_engfile_dir(engnode(H,_), H).
select_engfile_dir(engleaf(H), H).

find_engfile_tree(Dir, EngDirs) :-
    absolute_file_name(Dir, AbsDir),
    file_directory_name(AbsDir, Main),
    find_topmost_engfile_dir(Dir, Main, TopEng),
    file_directory_name(TopEng, TopDir),
    find_engfile_dirs(Dir, TopDir, EngDirs).
find_topmost_engfile_dir(Dir, InDir, Result) :-
    file_directory_name(InDir, ParentDir),
    \+ file_directory_name(ParentDir, ParentDir), % fail at root
    find_topmost_engfile_dir(Dir, ParentDir, Result).
find_topmost_engfile_dir(Dir, ParentDir, Result) :-
    directory_file_path(ParentDir, Dir, Result),
    exists_directory(Result).
find_engfile_dirs(TgtDir, Here, Tree) :-
    directory_files(Here, AllHere),
    atom_string(ATgtDir, TgtDir),
    findall(D, (member(E, AllHere),
                \+ member(E, [ '.', '..', ATgtDir,
                               '_darcs', '.git',
                               'dist-newstyle'
                             ]),
                directory_file_path(Here, E, Subdir),
                exists_directory(Subdir),
                find_engfile_dirs(TgtDir, Subdir, D)), SubTreeEnts),
    normalize_subtrees(SubTreeEnts, SubTree),
    find_engfile_here_subs(TgtDir, Here, SubTree, Tree).
find_engfile_here_subs(TgtDir, Here, [], engleaf(EngDir)) :-
    directory_file_path(Here, TgtDir, EngDir),
    exists_directory(EngDir),
    !.
find_engfile_here_subs(TgtDir, Here, Subs, engnode(EngDir, Subs)) :-
    directory_file_path(Here, TgtDir, EngDir),
    exists_directory(EngDir),
    !.
find_engfile_here_subs(_, _, Subs, Subs) :- !.

normalize_subtrees([], []).
normalize_subtrees([[]|ES], OS) :- !, normalize_subtrees(ES, OS).
normalize_subtrees([[E]|ES], [E|OS]) :- !, normalize_subtrees(ES, OS).
normalize_subtrees([E|ES], [E|OS]) :- normalize_subtrees(ES, OS).


ingest_engfiles(context(EngDir, TopDir), Refs) :-
    ingest_engfiles(context(EngDir, TopDir), Refs, informational).
ingest_engfiles(context(EngDir, TopDir), Refs, Verbosity) :-
    engfile_dir(Dir),
    find_engfile_dir(Dir, EngDir),
    file_directory_name(EngDir, TopDir),
    directory_files(EngDir, Files),
    ingest_files(Verbosity, EngDir, Files, Refs).

ingest_user_engfiles(Refs) :-
    absolute_file_name("~/.config/eng", UserConfigDir,
                       [ access(read),
                         file_type(directory),
                         file_errors(fail),
                         expand(true) ]),
    exists_directory(UserConfigDir), !,
    directory_files(UserConfigDir, Files),
    ingest_files(informational, UserConfigDir, Files, Refs).
ingest_user_engfiles([]).

ingest_files(Verbosity, Dir, Files, Refs) :-
    findall(R, ingest_files(Verbosity, Dir, Files, each, R), AllRefs),
    append(AllRefs, Refs).
ingest_files(Verbosity, Dir, Files, each, Refs) :-
    member(File, Files),
    directory_file_path(Dir, File, FilePath),
    ingest_file(Verbosity, FilePath, Refs).

ingest_file(Verbosity, File, Refs) :-
    % Process files with an .eng extension that aren't hidden files (start with a
    % period).
    file_name_extension(_, ".eng", File),
    file_base_name(File, Name), \+ string_chars(Name, ['.'|_]),
    % Read the file, parse it, and assert the facts in the file for predicate use
    % elsewhere.
    read_file_to_string(File, Contents, []),
    print_message(Verbosity, reading_eng_file(File)),
    parse_eng_eqil(File, Contents, Parsed),
    ( normalize_eqil(Parsed, Normalized), !,
      reprocess_eng_file(File, Normalized, Refs)
    ; (assert_eqil(Parsed, Refs), !
      ; print_message(error, eqil_nesting_too_deep(File)), Refs = []
      )
    ).

reprocess_eng_file(File, Updated_EQIL, Refs) :-
    !,
    emit_eqil(Updated_EQIL, OutText),
    string_concat(File, ".new", NewFile),
    open(NewFile, write, Out, [create([read, write]), type(text)]),
    format(Out, '~w~n', [OutText]),
    close(Out),
    rename_file(NewFile, File),
    print_message(informational, rewrote_eng_file(File)), !,
    parse_eng_eqil(File, OutText, Parsed),
    ( assert_eqil(Parsed, Refs), !
    ; print_message(error, eqil_nesting_too_deep(File)), Refs = []
    ).

erase_refs([]).
erase_refs([E|ES]) :- erase(E), erase_refs(ES).

prolog:message(reading_eng_file(File)) -->
    [ 'Ingesting ~w' - [File] ].
prolog:message(rewrote_eng_file(File)) -->
    [ 'Rewrote ~w' - [File] ].
prolog:message(eqil_nesting_too_deep(File)) -->
    [ 'Could not express ~w: maximum key nesting level depth exceeded ' - [File] ].
prolog:message(no_defined_subcmds(Cmd)) -->
    [ 'No currently user-defined "~w" sub-commands' - [ Cmd ] ].
prolog:message(invalid_subcmd(Cmd, context(EngDir, TopDir), SubCmd)) -->
    [ 'Invalid "~w" sub-command in ~w: ~w~n' - [ Cmd, TopDir, SubCmd ] ],
    {
        ingest_engfiles(context(EngDir, TopDir), Refs, silent),
        known_subcommands(Cmd, CS),
        erase_refs(Refs)
    },
    [ 'Valid sub-commands: ~w~n' - [ CS ] ].
prolog:message(invalid_subcmd(Cmd, SubCmd)) -->
    [ 'Invalid "~w" sub-command: ~w~n' - [ Cmd, SubCmd ] ],
    {
        known_subcommands(Cmd, CS)
    },
    [ 'Valid sub-commands: ~w~n' - [ CS ] ].
