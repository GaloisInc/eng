:- module(dependencies, [ sort_subprojects/3,
                          get_dependency_checker/4
                        ]).

%% This module provides the ability to detect the named dependencies of a
%% project, and to and order a set of projects such that the dependencies come
%% before the projects that depend on them.

:- use_module(library(apply)).
:- use_module('englib').
:- use_module('datafmts/cabal').
:- use_module(library(thread)).
:- use_module(library(yall)).

% Sorts the specified projects into a dependency sorted order (or simple
% alphanumeric order if no dependencies can be determined).  The entries in the
% input SubProjects list can be anything for which EntryInfo is a class instance
% (i.e. can be used upon).
%
% EntryInfo should be a dict with the following entries which are all predicates
% operating on the entries provided to sort_subprojects/3 and which should return
% the indicated information.
%
%  * entMyDepName: returns the Name to use for dependency checking and name-based
%    sorting.  this can be obtained from the depchecker or else it should be the
%    last SubDir path portion.
%
%  * entDepChecker: returns the DepChecker that can be called to check a
%    dependency

sort_subprojects(SubProjects, SortedSubProjects, EntryInfo) :-
    sort_subprojs(EntryInfo, SubProjects, [], [], SortedSubProjects).

sort_subprojs(ToInfo, [SC|SS], DepSort, Other, Ret) :-
    get_dict(entDepChecker, ToInfo, GetDepCheck),
    call(GetDepCheck, SC, _),
    !,
    % Insert this SC in dependency order
    insert_dep_subproj(ToInfo, SC, DepSort, DepSortWithS),
    sort_subprojs(ToInfo, SS, DepSortWithS, Other, Ret).
sort_subprojs(ToInfo, [S|SS], DS, Other, R) :-
    % No dependency checker: defer to end where this S will just be added
    % alphabetically.
    sort_subprojs(ToInfo, SS, DS, [S|Other], R).
sort_subprojs(ToInfo, [], DepSort, Rem, SortedSubProjects) :-
    % drain: all the primary entries have been processed, so now process the
    % postponed entries (which have no subdir and thus we cannot determine
    % dependencies) by adding them in simple alphabetical order without otherwise
    % perturbing the order so-far.
    foldl(insert_subprojs_alpha(ToInfo), Rem, DepSort, SortedSubProjects).
sort_subprojs(_, [], DepSort, [], DepSort).  % done

% Insert the entry *after* all of its dependencies in the list... actually, this
% scans the list and at the first entry in the list that is a dependency of S, S
% is added to the list just before that dependency element.
insert_dep_subproj(ToInfo, SC, [DC|DepSort], [SC,DC|DepSort]) :-
    get_dict(entMyDepName, ToInfo, ToName),
    call(ToName, SC, SName),
    get_dict(entDepChecker, ToInfo, GetDepCheck),
    call(GetDepCheck, DC, DepCheck),
    call(DepCheck, SName),
    !.  % yes, SC is a dependency of DC, so it needs to come before DC
insert_dep_subproj(ToInfo, SC, [DC|DepSort], [DC|SubDepSort]) :-
    % SC is not a dependency of DC, so it can follow DC
    insert_dep_subproj(ToInfo, SC, DepSort, SubDepSort).
insert_dep_subproj(_, SC, [], [SC]).


% Insert the entry in alphabetical order
insert_subprojs_alpha(_, S, [], [S]). % reached the end, add it there
insert_subprojs_alpha(EntryInfo, S, [D|Deps], [D|DepsWithS]) :-
    get_dict(entMyDepName, EntryInfo, EntryToName),
    call(EntryToName, D, DName),
    call(EntryToName, S, SName),
    compare('<', DName, SName),
    % keep looking
    insert_subprojs_alpha(EntryInfo, S, Deps, DepsWithS).
insert_subprojs_alpha(EntryInfo, S, [D|Deps], Deps) :-
    get_dict(entMyDepName, EntryInfo, EntryToName),
    call(EntryToName, D, DName),
    call(EntryToName, S, SName),
    % duplicate? unlikely, but if it happens, drop the duplicate
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
