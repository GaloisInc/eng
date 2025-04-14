:- module(cabal_project, [ write_cabal_project/2 ]).

:- use_module(library(filesex)).
:- use_module(library(url)).
:- use_module("../englib").
:- use_module("../commands/versionctl").

write_cabal_project(VCTool, InDir) :-
    absolute_file_name(InDir, TgtDir),
    working_directory(OldDir, TgtDir),
    relative_file_name(OldDir, TgtDir, RelPath),
    directory_file_path(TgtDir, "cabal.project", CProj),
    get_repos(Repos),
    get_local_repos(RelPath, Repos, Locals),
    get_remote_repos(VCTool, Repos, Remotes),
    get_extra(Extra),
    write_projfile(CProj, Repos, Locals, Remotes, Extra),
    print_message(informational, wrote_cabal_project(CProj)).

prolog:message(wrote_cabal_project(ProjFile)) -->
    [ 'Wrote cabal project file: ~w' - [ProjFile]].

get_repos(KnownRepos) :-
    findall(N, eng:key(vctl, subproject, N), KnownRepos).

get_local_repos(RelPath, KnownRepos, Locals) :-
    setof(X, get_local_repos_(RelPath, KnownRepos, X), LocalsList),
    append(LocalsList, Locals).
get_local_repos_(_, KnownRepos, Locals) :-
    setof((N,Dir), (member(N, KnownRepos),
                    vctl_subproj_local_dir(N, Dir),
                    exists_directory(Dir)), Locals).
get_local_repos_(RelPath, KnownRepos, Locals) :-
    setof((N,Dir), (member(N, KnownRepos),
                    vctl_subproj_local_dir(N, RDir),
                    directory_file_path(RelPath, RDir, Dir),
                    exists_directory(Dir)), Locals).

get_remote_repos(VCTool, KnownRepos, Remotes) :-
    findall((N,Rmt), (member(N, KnownRepos),
                      vctl_subproj_remote_repo(VCTool, N, Rmt)), Remotes).

get_extra(Extra) :-
    findall(O, (eng:eng(cabal_project, extra, N, V),
                format(atom(O), '~w~n  ~w~n', [N, V])), EPhrases),
    intercalate(EPhrases, "\n", Extra), !.

write_projfile(CProj, _Repos, Locals, Remotes, Extra) :-
    open(CProj, write, ProjFile),
    write_local_packages(ProjFile, Locals),
    write_extras(ProjFile, Extra),
    write_remotes(ProjFile, Locals, Remotes).

write_local_packages(ProjFile, Locals) :-
    findall((M,D), eng:eng(cabal_project, 'main packages', M, D), MainPkgs),
    append(MainPkgs, Locals, Pkgs),
    wlp(ProjFile, 'packages:', Pkgs).
wlp(ProjFile, Pfx, [(_,LDir)|Locals]) :-
    format(ProjFile, '~w ~w~n', [Pfx, LDir]),
    wlp(ProjFile, '         ', Locals).
wlp(_, _, []).

write_extras(ProjFile, Extra) :-
    format(ProjFile, '~n~w~n', [Extra]).

write_remotes(_, _, []).
write_remotes(ProjFile, Locals, [(N,_)|Remotes]) :-
    is_local(N, Locals), !, write_remotes(ProjFile, Locals, Remotes).
write_remotes(ProjFile, Locals, [(N,RmtRef)|Remotes]) :-
    wrp(ProjFile, N, RmtRef),
    wrp_reporef(ProjFile, N),
    wrp_subdir(ProjFile, N),
    write_remotes(ProjFile, Locals, Remotes).
wrp(ProjFile, _N, darcsremote(DarcsAddr)) :-
    format(ProjFile,
           '~nsource-repository-package~n  type: darcs~n  location: ~w~n',
           [ DarcsAddr ]),
    !.
wrp(ProjFile, _N, gitremote_ssh(S)) :-
    format(ProjFile,
           '~nsource-repository-package~n  type: git~n  location: ~w~n',
           [ S ]),
    !.
wrp(ProjFile, N, R) :-
    format(ProjFile, '~n-- TBD: write remote ~w @ ~w~n', [N, R]).
wrp_reporef(ProjFile, N) :-
    eng:eng(vctl, subproject, N, rev, V), !,
    format(ProjFile, '  tag: ~w~n', [V]).
wrp_reporef(_, _).
wrp_subdir(ProjFile, N) :-
    eng:eng(vctl, subproject, N, subdir, D), !,
    format(ProjFile, '  subdir: ~w~n', [D]).
wrp_subdir(_, _).

is_local(N, [(N,_)|_]).
is_local(N, [_|Lcls]) :- is_local(N, Lcls).
