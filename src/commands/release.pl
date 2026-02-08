:- module(release, [ release_cmd/3, release_focus/1, release_help/1, release_help/2 ]).

:- use_module(library(ansi_term)).
:- use_module(library(strings)).
:- use_module('../englib').
:- use_module(exec_subcmds).


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
|       prep =
|         {ExecSpecHelp}
|       post =
|         {ExecSpecHelp}
|
|}.

%% ----------------------------------------

release_subcmds([prep, post]).

release_help(prep, "Prepare a release.").
release_help(post, "Finish the release and return to development.").

release_cmd(_, [prep], sts(release, 1)) :- !, print_message(error, specify_release_version).
release_cmd(C, [prep,Version], S) :- do_release_prep(C, Version, S).
release_cmd(C, [post], S) :- do_release_post(C, S).
release_cmd(_, [Cmd|_], invalid_subcmd(releaseLY, Cmd)) :- !.

eng:use_dir(release, prep, '{TopDir}').

%% ----------------------------------------

do_release_prep(Context, Version, sts(prep, Sts)) :-
    eng:key(release, prep),
    !,
    set_env_vars(Context, [('NEWVERSION', Version)]),
    exec_from_spec_at(Context, [], [release, prep], Sts).
do_release_prep(_, _, sts(prep, 0)) :-
    \+ eng:key(release, prep),
    !.
    % print_message(info, no_release_prep_actions).

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
