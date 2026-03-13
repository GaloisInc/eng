:- module(release, [ release_cmd/3, release_focus/1, release_help/1, release_help/2 ]).

:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(readutil)).
:- use_module(library(strings)).
:- use_module('../englib').
:- use_module('../datafmts/eqil').
:- use_module(exec_subcmds).
:- use_module(versionctl).


release_focus("Releases").

release_help(Info) :-
    engfile_dir(EngDirV),
    exec_spec_help(8, ExecInfo),
    [EngDir, ExecSpecHelp] = [EngDirV, ExecInfo],
    Info = {|string(EngDir, ExecSpecHelp)||
| Engineering Release management.
|
| Provides functionality supporting release management.
|
| Release management information is specified in one or more
| .eng files in the {EngDir} directory of the project.  These
| files are typically in EQIL format (see the EQIL design
| document), and have the following structure:
|
|     release =
|       [exclude = SUBREPO [SUBREPO ...]]
|       prep =
|         [doc = PATH/TO/RELEASENOTES]
|         {ExecSpecHelp}
|       post =
|         {ExecSpecHelp}
|
| Preparing a release will run various executions, which frequently
| involves scripts to update version numbers and set version control
| tags, including other activities.  The NEWVERSION environment
| environment variable will be automatically set for prep executions
| to use for determining the intended release version.
|
| If the release notes doc is specified, then eng will suggest that
| any tasks with a current status of done that are not marked as
| internal visibility should be added to the release notes doc.
|
| It is strongly suggested that the release prep executions be setup
| such that it can be run repeatedly until the user is satisfied with
| the release preparation.
|
| If the optional "exclude" list is specified, the release operation
| will not be applied to any sub-repository whose name (as defined
| under "vctl/subproject") or local path matches one of the
| space-separated entries.  The top-level project is never excluded.
|}.

%% ----------------------------------------

release_subcmds([prep, post]).

release_help(prep, "Prepare a release.").
release_help(post, "Finish the release and return to development.").

release_cmd(_, [prep], sts(release, 1)) :- !, print_message(error, specify_release_version).
release_cmd(C, [prep,_Version], sts(prep, 0)) :- release_excluded(C), !.
release_cmd(C, [prep,Version], S) :- do_release_prep(C, Version, S).
release_cmd(C, [post], sts(post, 0)) :- release_excluded(C), !.
release_cmd(C, [post], S) :- do_release_post(C, S).
release_cmd(_, [Cmd|_], invalid_subcmd(release, Cmd)) :- !.

eng:use_dir(release, prep, '{TopDir}').

%% release_excluded(+Context) is true when the release operation should be
%% skipped for the given Context, based on the "release/exclude" entry in the
%% TOP-LEVEL project's eng files.  The top-level project (RelTip = '<here>') is
%% never excluded.
%%
%% Each subproject is processed with only its own eng files loaded (see top.pl),
%% so the "release/exclude" configuration -- which lives in the top-level project
%% -- must be loaded directly from the filesystem here.
%%
%% Exclusion is checked by matching the context's relative tip against the
%% space-separated names or local-directory paths listed in the exclude spec.

release_excluded(Context) :-
    context_reltip(Context, RelTip),
    RelTip \= '<here>',
    context_topdir(Context, TopDir),
    %% Recover TipTopDir: the overall project root.  By the invariant set up
    %% in ingest_engfiles (load.pl), directory_file_path(TipTopDir, RelTip, TopDir)
    %% always holds when RelTip \= '<here>', so atom_concat is the inverse.
    atom_concat('/', RelTip, RelSuffix),
    atom_concat(TipTopDir, RelSuffix, TopDir),
    %% Load the top-level project's eng files to check release/exclude
    engfile_dir(EngDirName),
    directory_file_path(TipTopDir, EngDirName, TipEngDir),
    exists_directory(TipEngDir),
    directory_files(TipEngDir, AllFiles),
    include([F]>>(file_name_extension(_, ".eng", F),
                  \+ string_chars(F, ['.'|_])),
             AllFiles, EngFiles),
    maplist([F, PR]>>(directory_file_path(TipEngDir, F, FP),
                      read_file_to_string(FP, FC, []),
                      parse_eng_eqil(FP, FC, PR)),
             EngFiles, Parsed),
    setup_call_cleanup(
        assert_eqil(Parsed, Refs),
        release_excluded_check(RelTip),
        maplist(erase, Refs)
    ).

%% True when the RelTip matches the release/exclude specification in the
%% currently loaded EQIL (the top-level project's).
release_excluded_check(RelTip) :-
    eng:eng(release, exclude, ExcludeSpec),
    split_string(ExcludeSpec, " ", "", ExcludeStrs),
    ExcludeStrs \= [],
    maplist(flip(atom_string), ExcludeStrs, Excludes),
    release_excluded_match(RelTip, Excludes).

%% True if RelTip matches any entry in Excludes, either directly (path match)
%% or by resolving a vctl subproject name to its local directory (name match).
release_excluded_match(RelTip, Excludes) :-
    member(RelTip, Excludes), !.
release_excluded_match(RelTip, Excludes) :-
    eng:key(vctl, subproject, Name),
    vctl_subproj_local_dir(Name, LclDir),
    LclDir = RelTip,
    member(Name, Excludes), !.

%% ----------------------------------------

do_release_prep(Context, Version, [sts(prep, Sts1), sts(prep, Sts2)]) :-
    eng:key(release, prep),
    !,
    do_release_prep_execs(Context, Version, Sts1),
    do_release_prep_notes(Context, Version, Sts2).
do_release_prep(_, _, sts(prep, 0)) :-
    \+ eng:key(release, prep),
    !.
    % print_message(info, no_release_prep_actions).

do_release_prep_execs(Context, Version, Sts) :-
    set_env_vars(Context, [('NEWVERSION', Version)]),
    exec_from_spec_at(Context, [], [release, prep], Sts).

do_release_prep_notes(Context, Version, 0) :-
    eng:eng(release, prep, doc, ReleaseDoc),
    !,
    context_topdir(Context, TopDir),
    directory_file_path(TopDir, ReleaseDoc, RlsDocPath),
    read_file_to_string(RlsDocPath, ReleaseNotes, []),
    findall((T, D, W),
            (eng:eng(tasks, Grp, T, status, "done"),
             eng:eng(tasks, Grp, T, summary, D),
             task_type(Grp, T, W),
             \+ eng:eng(tasks, Grp, T, visibility, "internal"),
             \+ string_contains(ReleaseNotes, T)
            ), TasksDone),
    tasks_done(ReleaseDoc, ReleaseNotes, Version, TasksDone).
do_release_prep_notes(_, _, 0).

task_type(G, T, W) :- eng:eng(tasks, G, T, type, W), !.
task_type(_, _, "-").

tasks_done(_, _, _, []) :- !.
tasks_done(ReleaseDoc, ReleaseNotes, Version, TDS) :-
    print_message(info, tasks_not_in_release_notes(ReleaseDoc, Version)),
    tasks_done_(ReleaseDoc, ReleaseNotes, Version, TDS).
tasks_done_(ReleaseDoc, ReleaseNotes, Version, [(T,D, W)|TDS]) :-
    task_done(ReleaseDoc, ReleaseNotes, Version, T, D, W),
    tasks_done_(ReleaseDoc, ReleaseNotes, Version, TDS).
tasks_done_(_, _, _, []).

task_done(_RlsDoc, _RlsNotes, V, T, D, W) :-
    print_message(info, task_done_for_release(V, T, D, W)).


%% ----------------------------------------

do_release_post(Context, sts(release, Sts)) :-
    eng:key(release, post),
    exec_from_spec_at(Context, [], [release, post], Sts).
do_release_post(_, sts(release, 0)) :-
    \+ eng:key(release, post),
    !.
    % print_message(info, no_release_post_actions).

prolog:message(specify_release_version) -->
    [ 'Please specify the release version to assign' ].
prolog:message(no_release_prep_actions) -->
    [ 'No release preparation actions defined' ].
prolog:message(no_release_post_actions) -->
    [ 'No post-release actions defined' ].
prolog:message(tasks_not_in_release_notes(D, V)) -->
    [ 'The following tasks were completed in ~w but are not~n' - V,
      'mentioned in the release notes in ~w:' - [D]
    ].
prolog:message(task_done_for_release(_V, T, D, W)) -->
    [ '  ~w task ~w - ~w' - [W, T, D] ].
