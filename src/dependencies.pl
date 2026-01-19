:- module(deps, [ vctl_subprojects/2,
                  vctl_subproj_local_dir/2
                ]).

%% This module provides the ability to detect the named dependencies of a
%% project, and to and order a set of projects such that the dependencies come
%% before the projects that depend on them.
%%
%% Although most of the functionality in this module is very general, it does
%% require that the eqil vctl subproject specifications have been asserted, and
%% this file provides a few helpers used by the versionctl command
%% implementation.

:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module('englib').
:- use_module('datafmts/cabal').
:- use_module(library(thread)).
:- use_module(library(yall)).

% Get the local directory to use for a subproject.
vctl_subproj_local_dir(Name, LclDir) :-
    eng:eng(vctl, subproject, Name, into, LclDir), !.
vctl_subproj_local_dir(Name, LclDir) :-
    eng:key(vctl, subproject, Name),
    directory_file_path(subproj, Name, LclDir), !.
vctl_subproj_local_dir(Name, LclDir) :-
    directory_file_path(subproj, Name, LclDir).


% Get the subprojects (if any), ordered so that dependencies appear before their
% user
vctl_subprojects(Context, SubProjects) :-
    setof(S, eng:key(vctl, subproject, S), SPS),
    concurrent_maplist(get_projdeps(Context), SPS, SPDS),
    sort_subprojects(SPDS, SortedSubProjects),
    !,
    concurrent_maplist(getname, SortedSubProjects, SubProjects).

get_projdeps(Context, SName, subproj_and_depcheck(SName, Checker)) :-
    vctl_subproj_local_dir(SName, SSub),
    context_subdir(Context, SSub, SDir),
    get_dependency_checker(Context, SDir, Checker).
get_projdeps(_, SName, subproj_no_depcheck(SName)).

getname(subproj_and_depcheck(N, _), N).
getname(subproj_no_depcheck(N), N).

% /3 (primary)
sort_subprojects(SPS, SubProjects) :-
    sort_subprojects(SPS, SubProjects, getname).

% /4 (helper: launch)
sort_subprojects(SubProjects, SortedSubProjects, EntryToName) :-
    sort_subprojs(EntryToName, SubProjects, [], [], SortedSubProjects).

% /6 (helper: worker)
sort_subprojs(N, [subproj_no_depcheck(S)|SS], DS, Other, R) :-
    % No dependency checker: defer to end where this S will just be added
    % alphabetically.
    sort_subprojs(N, SS, DS, [subproj_no_depcheck(S)|Other], R).
sort_subprojs(ToName, [SC|SS], DepSort, Other, Ret) :-
    % Insert this SC = subproj_and_depcheck(S,C) in dependency order
    insert_dep_subproj(ToName, SC, DepSort, DepSortWithS),
    sort_subprojs(ToName, SS, DepSortWithS, Other, Ret).
sort_subprojs(ToName, [], DepSort, Rem, SortedSubProjects) :-
    % drain: all the primary entries have been processed, so now process the
    % postponed entries (which have no subdir and thus we cannot determine
    % dependencies) by adding them in simple alphabetical order without otherwise
    % perturbing the order so-far.
    foldl(insert_subprojs_alpha(ToName), Rem, DepSort, SortedSubProjects).
sort_subprojs(_, [], DepSort, [], DepSort).  % done

% Insert the entry *after* all of its dependencies in the list... actually, this
% scans the list and at the first entry in the list that is a dependency of S, S
% is added to the list just before that dependency element.
insert_dep_subproj(_, SC, [], [SC]).
insert_dep_subproj(ToName, SC, [DC|DepSort], [SC,DC|DepSort]) :-
    call(ToName, SC, SName),
    DC = subproj_and_depcheck(_, DDepCheck),
    call(DDepCheck, SName),
    !.  % yes, SC is a dependency of DC, so it needs to come before DC
insert_dep_subproj(ToName, SC, [DC|DepSort], [DC|SubDepSort]) :-
    insert_dep_subproj(ToName, SC, DepSort, SubDepSort).

% Insert the entry in alphabetical order
insert_subprojs_alpha(_, S, [], [S]). % reached the end, add it there
insert_subprojs_alpha(EntryToName, S, [D|Deps], [D|DepsWithS]) :-
    call(EntryToName, D, DName),
    call(EntryToName, S, SName),
    compare('<', DName, SName),
    % keep looking
    insert_subprojs_alpha(EntryToName, S, Deps, DepsWithS).
insert_subprojs_alpha(EntryToName, S, [D|Deps], Deps) :-
    call(EntryToName, D, DName),
    call(EntryToName, S, SName),
    % duplicate? drop
    compare('=', DName, SName).
insert_subprojs_alpha(_, S, Deps, [S|Deps]).  % found the spot to insert

% ----------------------------------------------------------------------

get_dependency_checker(Context, ProjDir, MyName, DepChecker) :-
    exists_context_subdir(Context, ProjDir),
    context_subdir(Context, ProjDir, D),
    % Find various things that we can use to determine dependencies
    find_dependency_checker(Context, D, MyName, DepChecker).

find_dependency_checker(_Context, D, MyName, DepChecker) :-
    find_cabal_dependency_checker(D, MyName, DepChecker).
