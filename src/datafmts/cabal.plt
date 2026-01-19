:- begin_tests(cabal).
:- use_module(cabal).
:- use_module('../englib').
:- use_module(library(strings)).


test(simple_name, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| name: me
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_pkgname(Name), Input, _),
    string_codes(NameS, Name),
    assertion(NameS == "me").

test(complex_name, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| name: my-name_is_mudd
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_pkgname(Name), Input, _),
    string_codes(NameS, Name),
    assertion(NameS == "my-name_is_mudd").

test(name_and_comment, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| name: my-name -- is mudd
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_pkgname(Name), Input, _),
    string_codes(NameS, Name),
    assertion(NameS == "my-name").

test(simple_dep, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| build-depends: dep
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes(DepS, Dep),
    assertion(DepS == "dep").

test(simple_dep_check, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| build-depends: dep1
|},
    writeln(Inp),
    string_codes(Inp, Input),
    string_codes("dep1", Dep),
    phrase(cabal:cabal_build_dependency(Dep), Input, _).

test(simple_dep_with_following_subphrase, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| build-depends: dep
| after: more junk
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes(DepS, Dep),
    assertion(DepS == "dep").

test(simple_subdep, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: dep
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes(DepS, Dep),
    assertion(DepS == "dep").

test(simple_dep_with_following_subphrase, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: dep
|
| after: more junk
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes(DepS, Dep),
    assertion(DepS == "dep").

test(simple_dep_with_comment, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: dep -- my dependencies
|
| after: more junk
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes(DepS, Dep),
    assertion(DepS == "dep").

test(simple_dep_with_empty_comment, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: dep --
|
| after: more junk
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes(DepS, Dep),
    assertion(DepS == "dep").

test(multi_deps_with_following_subphrase, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: dep, otherdep
|
| after: more junk
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes(DepS, Dep),
    assertion(DepS == "dep").

test(following_deps, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: dep, otherdep, finaldep
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes(DepS, Dep),
    assertion(DepS == "dep").

test(middle_dep, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: otherdep, dep, finaldep
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes("dep", Dep).

test(third_dep, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: first_dep, otherdep, dep, finaldep
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes("dep", Dep).

test(final_dep, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: otherdep, finaldep, dep
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes("dep", Dep).

test(ignore_non_dep, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: first_dep, otherdep, finaldep
|   other: dep, dep, dep
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    (string_codes("dep", Dep), !, fail ; true).

test(full_dep, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: first_dep, dep_more, san_dep_wich, finaldep
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    (string_codes("dep", Dep), !, fail ; true).

test(every_dep, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: otherdep, dep, finaldep
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes("dep", Dep),
    phrase(cabal:cabal_build_dependency(Dep2), Input, _),
    string_codes("otherdep", Dep2),
    phrase(cabal:cabal_build_dependency(Dep3), Input, _),
    string_codes("finaldep", Dep3).

test(every_dep_multiple_lines, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: otherdep,
|                  dep,
|                  third-dep
|                , finaldep
|   other: stuff
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes("dep", Dep),
    phrase(cabal:cabal_build_dependency(Dep2), Input, _),
    string_codes("otherdep", Dep2),
    phrase(cabal:cabal_build_dependency(Dep3), Input, _),
    string_codes("third-dep", Dep3),
    phrase(cabal:cabal_build_dependency(Dep4), Input, _),
    string_codes("finaldep", Dep4).

test(simple_dep_with_equality_constraint, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: dep == 3.3
|
| after: more junk
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes(DepS, Dep),
    assertion(DepS == "dep").

test(every_dep_multiple_lines_with_constraints, [nondet]) :-
    Inp = {|string||
| cabal-version: 2.2
| library:
|   build-depends: otherdep == 3,
|                  dep>=3.1 && < 4,
|                  third-dep ^>= 6.6
|                , finaldep -any
|   other: stuff
|},
    writeln(Inp),
    string_codes(Inp, Input),
    phrase(cabal:cabal_build_dependency(Dep), Input, _),
    string_codes("dep", Dep),
    phrase(cabal:cabal_build_dependency(Dep2), Input, _),
    string_codes("otherdep", Dep2),
    phrase(cabal:cabal_build_dependency(Dep3), Input, _),
    string_codes("third-dep", Dep3),
    phrase(cabal:cabal_build_dependency(Dep4), Input, _),
    string_codes("finaldep", Dep4).
