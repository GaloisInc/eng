:- module(vctl_subproj_assist, [ vctl_subprojects/2,
                                 vctl_local_subprojects/3,
                                 vctl_subprojects_and_lcldirs/2,
                                 vctl_subproj_preface/2,
                                 vctl_subproj_context/3,
                                 vctl_subproj_context_has_engfiles/1,
                                 vctl_subproj_local_dir/2
                               ]).


% Get the subprojects relative to the input Context (if any), ordered so that
% dependencies appear before their user.
vctl_subprojects(Context, SubProjects) :-
    setof(S, eng:key(vctl, subproject, S), SPS),
    concurrent_maplist(get_projdeps(Context), SPS, SPDS),
    EInfo = entryinfo{ entMyDepName: vctl_subproj_assist:get_sp_name,
                       entDepChecker: vctl_subproj_assist:get_sp_checker
                     },
    sort_subprojects(SPDS, SortedSubProjects, EInfo),
    !,
    % Now get back the plain entries from the enhanced sorted entries:
    concurrent_maplist(get_sp_entry, SortedSubProjects, SubProjects),
    true.

get_projdeps(Context, SubProj, subproj_and_depcheck(SubProj, Name, Checker)) :-
    vctl_subproj_local_dir(SubProj, SSub),
    context_subdir(Context, SSub, SDir),
    get_dependency_checker(Context, SDir, Name, Checker).
get_projdeps(_, SubProj, subproj_no_depcheck(SubProj, Name)) :-
    file_base_name(SubProj, Name).

get_sp_name(subproj_and_depcheck(_, N, _), N).
get_sp_name(subproj_no_depcheck(_, N), N).

get_sp_checker(subproj_and_depcheck(_, _, C), C).

get_sp_entry(subproj_and_depcheck(E,_,_), E).
get_sp_entry(subproj_no_depcheck(E,_), E).


% Get the subprojects and the local directory (whether it exists or not).
% Ordered as per vctl_subprojects.
vctl_subprojects_and_lcldirs(Context, SubProjectsAndDirs) :-
    vctl_subprojects(Context, SP),
    vsal(SP, SubProjectsAndDirs).
vsal([], []).
vsal([S|SS], [(S,D)|SPDS]) :- vctl_subproj_local_dir(S, D), !,
                              vsal(SS, SPDS).
vsal([_|SS], SPDS) :- vsal(SS, SPDS).

% Get the subprojects and the local directory, eliding entries that do not have a
% local directory.  Ordered as per vctl_subprojects.
vctl_local_subprojects(Context, SubProjects, LocalSubProjects) :-
    vls(Context, SubProjects, LocalSubProjects).
vls(_, [], []).
vls(Context, [S|SS], [(S,D)|SPDS]) :- vctl_subproj_local_dir(S, D),
                                      exists_context_subdir(Context, D),
                                      !,
                                      vls(Context, SS, SPDS).
vls(Context, [_|SS], SPDS) :- vls(Context, SS, SPDS).

% Get the preface to use for printing information about the named subproj
vctl_subproj_preface(Name, Preface) :-
    format(atom(Preface), '#____ ~w :: ', [Name]).

vctl_subproj_context(Context, SubProjDir,
                     context(EngDir, SPDir, SubProjDir)) :-
    context_topdir(Context, TopDir),
    context_engdir(Context, EngDir),
    directory_file_path(TopDir, SubProjDir, SPDir).

vctl_subproj_context_has_engfiles(Context) :-
    context_topdir(Context, SPDir),
    has_engfiles(SPDir, _).

% Get the local directory to use for a subproject.
vctl_subproj_local_dir(Name, LclDir) :-
    eng:eng(vctl, subproject, Name, into, LclDir), !.
vctl_subproj_local_dir(Name, LclDir) :-
    eng:key(vctl, subproject, Name),
    directory_file_path(subproj, Name, LclDir), !.
vctl_subproj_local_dir(Name, LclDir) :-
    directory_file_path(subproj, Name, LclDir).
