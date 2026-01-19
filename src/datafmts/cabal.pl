:- module(cabal, [ find_cabal_dependency_checker/3 ]).

:- use_module(library(dcg/basics)).

find_cabal_dependency_checker(D, MyName, DepChecker) :-
    % This one will use a .cabal file if it exists
    directory_member(D, CabalFile, [extensions(['cabal'])]),
    cabal_dependency_checker(CabalFile, MyName, DepChecker).

cabal_dependency_checker(CabalFile, MyName, DepChecker) :-
    read_file_to_codes(CabalFile, CabalInfo, []),
    phrase(cabal_pkgname(MyNameCodes), CabalInfo, _),
    atom_codes(MyName, MyNameCodes),
    DepChecker = ({CabalInfo}/[SubProjName] >>
                  (atom_codes(SubProjName, Dep),
                   phrase(cabal:cabal_build_dependency(Dep), CabalInfo, _),
                   ! % found it, so no need to retry alternate phrase results
                   )).

cabal_pkgname(Name) --> preceeding(_),
                        subphrase("name"),
                        simple_name(Name),
                        name_end.

simple_name(Name) --> whites,
                      string_without(": ,<>=^\n\t()!@#$%^*\\|[]{}?/", Name),
                      whites.

name_end --> whites, (eol; comment; eos).
comment -->  "--", string(_), (eol; eos).

cabal_build_dependency(Dep) --> preceeding(_),
                                subphrase("build-depends"),
                                build_dep(Dep).

subphrase(Name) --> "\n", whites,
                    string_without(":", S),
                    {string_codes(S, Name)},
                    ":".

build_dep(Dep) --> simple_name(Dep), list_name_end.
build_dep(Dep) --> list_entry_start, simple_name(Dep), list_name_end.
build_dep(Dep) --> list_entry_start, blanks_to_nl, simple_name(Dep), list_name_end.

list_entry_start --> string(_), (":", !, {fail}; ",").
list_name_end --> whites, (eol ; eos ; ","; comment ; version_constraint_rel).

version_constraint_rel --> (">=" ; "<="; "^>="; "=="; ">"; "<"; "-any").

preceeding(Stuff) --> string(Stuff).
following(Stuff) --> string(Stuff).
