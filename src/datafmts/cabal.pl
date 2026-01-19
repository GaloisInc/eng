:- module(cabal, [ find_cabal_dependency_checker/3 ]).

:- use_module(library(dcg/basics)).

find_cabal_dependency_checker(D, MyName, DepChecker) :-
    % This one will use a .cabal file if it exists
    directory_member(D, CabalFile, [extensions(['cabal'])]),
    cabal_dependency_checker(CabalFile, MyName, DepChecker).

cabal_dependency_checker(CabalFile, MyName, DepChecker) :-
    read_file_to_codes(CabalFile, CabalInfo, []),
    % FIXME get actual name from CabalFile; here we just use the directory name
    file_directory_name(CabalFile, CabalDir),
    file_base_name(CabalDir, MyName),

    DepChecker = ({MyName,CabalInfo}/[SubProjName] >>
                  (atom_string(SubProjName, SPN),
                   string_codes(SPN, Dep),
                   phrase(cabal:cabal_build_dependency(Dep), CabalInfo, _),
                   ! % found it, so no need to retry alternate phrase results
                   )).

cabal_build_dependency(Dep) --> preceeding(_),
                                subphrase("build-depends"),
                                build_dep(Dep),
                                (subphrase(_) ; eol).

subphrase(Name) --> "\n", whites,
                    string_without(":", S),
                    {string_codes(S, Name)},
                    ":".

build_dep(Dep) --> whites, string_without(": ,<>=^", Dep), string(_).
build_dep(Dep) --> string(_), ",", blanks, string_without(": ,<>=^", Dep), string(_).
build_dep(Dep) --> string(_), ",", "\n", whites, string_without(": ,<>=^", Dep), string(_).


preceeding(Stuff) --> string(Stuff).
following(Stuff) --> string(Stuff).
