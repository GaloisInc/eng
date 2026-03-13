:- begin_tests(release).
:- use_module(release).
:- use_module('../englib').

%% Helper: retract all eng:key and eng:eng dynamic facts asserted during a test.
%% Uses the same name as the equivalent helper in eqil.plt.
revert_assert_eng :-
    retractall(eng:key(_)),
    retractall(eng:key(_,_)),
    retractall(eng:key(_,_,_)),
    retractall(eng:key(_,_,_,_)),
    retractall(eng:key(_,_,_,_,_)),
    retractall(eng:key(_,_,_,_,_,_)),
    retractall(eng:key(_,_,_,_,_,_,_)),
    retractall(eng:eng(_,_)),
    retractall(eng:eng(_,_,_)),
    retractall(eng:eng(_,_,_,_)),
    retractall(eng:eng(_,_,_,_,_)),
    retractall(eng:eng(_,_,_,_,_,_)),
    retractall(eng:eng(_,_,_,_,_,_,_)),
    retractall(eng:eng(_,_,_,_,_,_,_,_)).

%% --- release_excluded/1 tests ---

%% When no "release/exclude" key is defined, release_excluded/1 fails.
test(no_exclude_config, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    Context = context('_eng_', '/project', 'subproj/foo'),
    assertion(\+ release_excluded(Context)).

%% The top-level context (RelTip = '<here>') is never excluded, even if
%% the exclude list is present.
test(toplevel_not_excluded, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    assertz(eng:key(release, exclude)),
    assertz(eng:eng(release, exclude, "foo")),
    Context = context('_eng_', '/project', '<here>'),
    assertion(\+ release_excluded(Context)).

%% A subproject whose local path exactly matches an entry in the exclude list
%% is excluded (path-based matching).
test(exclude_by_path, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    assertz(eng:key(release, exclude)),
    assertz(eng:eng(release, exclude, "subproj/foo")),
    Context = context('_eng_', '/project/subproj/foo', 'subproj/foo'),
    assertion(release_excluded(Context)).

%% A subproject whose vctl subproject name matches an entry in the exclude list
%% is excluded (name-based matching via vctl_subproj_local_dir).
test(exclude_by_name, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    assertz(eng:key(vctl, subproject, foo)),
    assertz(eng:key(release, exclude)),
    assertz(eng:eng(release, exclude, "foo")),
    %% With no "into" override, the local dir defaults to subproj/foo
    Context = context('_eng_', '/project/subproj/foo', 'subproj/foo'),
    assertion(release_excluded(Context)).

%% A subproject not in the exclude list is not excluded.
test(not_in_exclude_list, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    assertz(eng:key(release, exclude)),
    assertz(eng:eng(release, exclude, "other")),
    Context = context('_eng_', '/project/subproj/foo', 'subproj/foo'),
    assertion(\+ release_excluded(Context)).

%% Multiple entries in the exclude list: matching names are excluded.
test(multiple_excludes_match, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    assertz(eng:key(release, exclude)),
    assertz(eng:eng(release, exclude, "bar subproj/foo")),
    Context = context('_eng_', '/project/subproj/foo', 'subproj/foo'),
    assertion(release_excluded(Context)).

%% Multiple entries in the exclude list: non-matching contexts are not excluded.
test(multiple_excludes_no_match, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    assertz(eng:key(release, exclude)),
    assertz(eng:eng(release, exclude, "bar baz")),
    Context = context('_eng_', '/project/subproj/foo', 'subproj/foo'),
    assertion(\+ release_excluded(Context)).

%% An explicit "into" path in the vctl subproject spec is used for name
%% matching when the path differs from the default subproj/NAME.
test(exclude_by_name_with_into, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    assertz(eng:key(vctl, subproject, mypkg)),
    assertz(eng:key(vctl, subproject, mypkg, into)),
    assertz(eng:eng(vctl, subproject, mypkg, into, 'deps/mypkg')),
    assertz(eng:key(release, exclude)),
    assertz(eng:eng(release, exclude, "mypkg")),
    Context = context('_eng_', '/project/deps/mypkg', 'deps/mypkg'),
    assertion(release_excluded(Context)).

%% --- release_cmd/3 integration tests ---

%% release_cmd prep with an excluded context returns success (0) without running.
test(release_cmd_prep_excluded, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    assertz(eng:key(release, exclude)),
    assertz(eng:eng(release, exclude, "subproj/foo")),
    Context = context('_eng_', '/project/subproj/foo', 'subproj/foo'),
    release_cmd(Context, [prep, '1.0'], Sts),
    assertion(Sts == sts(prep, 0)).

%% release_cmd post with an excluded context returns success (0) without running.
test(release_cmd_post_excluded, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    assertz(eng:key(release, exclude)),
    assertz(eng:eng(release, exclude, "subproj/foo")),
    Context = context('_eng_', '/project/subproj/foo', 'subproj/foo'),
    release_cmd(Context, [post], Sts),
    assertion(Sts == sts(post, 0)).

%% release_cmd prep with a non-excluded context and no release/prep defined
%% returns success (0), preserving prior behaviour.
test(release_cmd_prep_no_excludes_no_prep, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    %% No release/exclude and no release/prep defined.
    Context = context('_eng_', '/project', '<here>'),
    release_cmd(Context, [prep, '1.0'], Sts),
    assertion(Sts == sts(prep, 0)).

%% release_cmd post with a non-excluded context and no release/post defined
%% returns success (0), preserving prior behaviour.
test(release_cmd_post_no_excludes_no_post, [nondet, cleanup(revert_assert_eng)]) :-
    revert_assert_eng,
    %% No release/exclude and no release/post defined.
    Context = context('_eng_', '/project', '<here>'),
    release_cmd(Context, [post], Sts),
    assertion(Sts == sts(release, 0)).

:- end_tests(release).
