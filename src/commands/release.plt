:- begin_tests(release).
:- use_module(release).
:- use_module('../englib').
:- use_module(library(filesex)).

%% Creates a temp directory structure for release_excluded tests:
%%   TipTopDir/_eng_/test.eng  (with EqilContent, the top-level eng config)
%%   TipTopDir/RelTip/         (subproject directory)
%% Returns TipTopDir for cleanup and a consistent Context for test use.
%% EqilContent must be a non-empty string in EQIL format.
make_release_test_env(RelTip, EqilContent, TipTopDir, Context) :-
    tmp_file(eng_release_test, TipTopDir),
    make_directory(TipTopDir),
    directory_file_path(TipTopDir, '_eng_', TipEngDir),
    make_directory(TipEngDir),
    directory_file_path(TipEngDir, 'test.eng', EngFile),
    setup_call_cleanup(
        open(EngFile, write, S),
        format(S, '~w', [EqilContent]),
        close(S)),
    directory_file_path(TipTopDir, RelTip, SubDir),
    make_directory_path(SubDir),
    Context = context('_eng_', SubDir, RelTip).

%% --- release_excluded/1 tests ---

%% When no "release/exclude" key is defined in the top-level EQIL,
%% release_excluded/1 fails.
test(no_exclude_config,
     [nondet,
      setup(make_release_test_env('subproj/foo', "other = value\n",
                                  TipTopDir, Context)),
      cleanup(delete_directory_and_contents(TipTopDir))]) :-
    assertion(\+ release_excluded(Context)).

%% The top-level context (RelTip = '<here>') is never excluded.
%% release_excluded/1 short-circuits before any filesystem access for this case.
test(toplevel_not_excluded, [nondet]) :-
    Context = context('_eng_', '/project', '<here>'),
    assertion(\+ release_excluded(Context)).

%% A subproject whose local path exactly matches an entry in the exclude list
%% is excluded (path-based matching).
test(exclude_by_path,
     [nondet,
      setup(make_release_test_env('subproj/foo',
                                  "release =\n  exclude = subproj/foo\n",
                                  TipTopDir, Context)),
      cleanup(delete_directory_and_contents(TipTopDir))]) :-
    assertion(release_excluded(Context)).

%% A subproject whose vctl subproject name matches an entry in the exclude list
%% is excluded (name-based matching via vctl_subproj_local_dir).
%% With no "into" override the local dir defaults to subproj/NAME.
test(exclude_by_name,
     [nondet,
      setup(make_release_test_env('subproj/foo',
                                  "vctl =\n  subproject =\n    foo =\n\nrelease =\n  exclude = foo\n",
                                  TipTopDir, Context)),
      cleanup(delete_directory_and_contents(TipTopDir))]) :-
    assertion(release_excluded(Context)).

%% A subproject not in the exclude list is not excluded.
test(not_in_exclude_list,
     [nondet,
      setup(make_release_test_env('subproj/foo',
                                  "release =\n  exclude = other\n",
                                  TipTopDir, Context)),
      cleanup(delete_directory_and_contents(TipTopDir))]) :-
    assertion(\+ release_excluded(Context)).

%% Multiple entries in the exclude list: matching names are excluded.
test(multiple_excludes_match,
     [nondet,
      setup(make_release_test_env('subproj/foo',
                                  "release =\n  exclude = bar subproj/foo\n",
                                  TipTopDir, Context)),
      cleanup(delete_directory_and_contents(TipTopDir))]) :-
    assertion(release_excluded(Context)).

%% Multiple entries in the exclude list: non-matching contexts are not excluded.
test(multiple_excludes_no_match,
     [nondet,
      setup(make_release_test_env('subproj/foo',
                                  "release =\n  exclude = bar baz\n",
                                  TipTopDir, Context)),
      cleanup(delete_directory_and_contents(TipTopDir))]) :-
    assertion(\+ release_excluded(Context)).

%% An explicit "into" path in the vctl subproject spec is used for name
%% matching when the path differs from the default subproj/NAME.
test(exclude_by_name_with_into,
     [nondet,
      setup(make_release_test_env('deps/mypkg',
                                  "vctl =\n  subproject =\n    mypkg =\n      into = deps/mypkg\n\nrelease =\n  exclude = mypkg\n",
                                  TipTopDir, Context)),
      cleanup(delete_directory_and_contents(TipTopDir))]) :-
    assertion(release_excluded(Context)).

%% --- release_cmd/3 integration tests ---

%% release_cmd prep with an excluded context returns success (0) without running.
test(release_cmd_prep_excluded,
     [nondet,
      setup(make_release_test_env('subproj/foo',
                                  "release =\n  exclude = subproj/foo\n",
                                  TipTopDir, Context)),
      cleanup(delete_directory_and_contents(TipTopDir))]) :-
    release_cmd(Context, [prep, '1.0'], Sts),
    assertion(Sts == sts(prep, 0)).

%% release_cmd post with an excluded context returns success (0) without running.
test(release_cmd_post_excluded,
     [nondet,
      setup(make_release_test_env('subproj/foo',
                                  "release =\n  exclude = subproj/foo\n",
                                  TipTopDir, Context)),
      cleanup(delete_directory_and_contents(TipTopDir))]) :-
    release_cmd(Context, [post], Sts),
    assertion(Sts == sts(post, 0)).

%% release_cmd prep with a non-excluded context and no release/prep defined
%% returns success (0), preserving prior behaviour.
test(release_cmd_prep_no_excludes_no_prep, [nondet]) :-
    revert_assert_eng,
    %% No release/exclude and no release/prep defined.
    Context = context('_eng_', '/project', '<here>'),
    release_cmd(Context, [prep, '1.0'], Sts),
    assertion(Sts == sts(prep, 0)).

%% release_cmd post with a non-excluded context and no release/post defined
%% returns success (0), preserving prior behaviour.
test(release_cmd_post_no_excludes_no_post, [nondet]) :-
    revert_assert_eng,
    %% No release/exclude and no release/post defined.
    Context = context('_eng_', '/project', '<here>'),
    release_cmd(Context, [post], Sts),
    assertion(Sts == sts(release, 0)).

:- end_tests(release).
