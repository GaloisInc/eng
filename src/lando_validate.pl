:- module(lando_validate, [ validate_lando/2 ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(http/json)).
:- use_module(src/datafmts/lando).
:- use_module(englib).


validate_lando(SSL, ValidationErrors) :-
    get_dict(body, SSL, Body),
    findall(E, judgement_4(Body, E), Errs4),
    findall(E, judgement_5(Body, E), Errs5),
    findall(E, judgement_23(Body, E), Errs23),
    append(Errs4, Errs4_),
    append(Errs5, Errs5_),
    append(Errs23, Errs23_),
    append([ Errs4_, Errs5_, Errs23_ ], ValidationErrors).

judgement_4(SSL, [Error]) :-
    findall(S, (member(S, SSL), is_dict(S, system)), Systems),
    length(Systems, N),
    N > 1,
    maplist(get_dict(name), Systems, Names),
    format(atom(Error), 'Only one top-level system allowed, found ~w: ~w',
           [ N, Names ]).
% All other judgement 4 preconditions identical to judgement 5

judgement_5(SSL, [Error]) :-
    % Precondition 1: name and abbrevName are different
    member(S, SSL),
    is_dict(S, system),
    get_dict(name, S, N),
    get_dict(abbrevName, S, A),  % A could be null, but still should not match N
    N == A,
    format(atom(Error), 'system name and abbrevName should not match: ~w',
           [ N ]).
% Precondition 2: explanation text well-formedness: handled by parser
% Precondition 3: valid elements in system: handled by parser
judgement_5(SSL, Errors) :-
    % Precondition 4: elements of System are valid
    member(S, SSL),
    is_dict(S, system),
    valid_contains(SSL, system, S, Errors).
% Precondition 5: system is well-formed inside environment: TODO check references

% Judgement 21: Relation well formed
% judgement_21(SSL, relation, E, [Err]) :-
% get_dict(name, E, Name),
% \+ resolve_qualname(SSL, Name),
% format(atom(Err), 'Relation ~w qualified name references no valid element',
% [ Name ]),
judgement_21(SSL, relation, E, Errors) :-
    get_dict(inherits, E, SubElements),
    get_dict(name, E, Name),
    valid_elements(SSL, relation, Name, SubElements, ElementsErrors),
    append(ElementsErrors, Errors).
judgement_21(SSL, relation, E, Errors) :-
    get_dict(clientOf, E, SubElements),
    get_dict(name, E, Name),
    valid_elements(SSL, relation, Name, SubElements, ElementsErrors),
    append(ElementsErrors, Errors).


% Judgement 23: Valid top-level (probably handled by parsing)
judgement_23([], []).
judgement_23([E|ES], Errors) :-
    specElement_type(E, EType),
    member(EType, ["System", "Subsystem", "Component",
                   "Events", "Scenarios", "Requirements", "Relation" ]), !,
    judgement_23(ES, Errors).
judgement_23(system, _Name, [E|ES], [Err|Errors]) :-
    specElement_type(E, EType),
    format(atom(Err), "Invalid top-level element: ~w", [ EType ]),
    judgement_23(ES, Errors).


valid_contains(SSL, system, System, Errors) :-
    get_dict(body, System, SubElements),
    get_dict(name, System, Name),
    valid_elements(SSL, system, Name, SubElements, ElementsErrors),
    append(ElementsErrors, Errors).
valid_contains(SSL, subsystem, SubSystem, Errors) :-
    get_dict(body, SubSystem, SubElements),
    get_dict(name, SubSystem, Name),
    valid_elements(SSL, subsystem, Name, SubElements, ElementsErrors),
    append(ElementsErrors, Errors).

% Judgement 21: Relation well formed
valid_elements(_SSL, relation, _, [], []).
valid_elements(SSL, relation, Name, [QualName|QualNames], Errors) :-
    resolve_qualname(SSL, QualName), !,
    valid_elements(SSL, relation, Name, QualNames, Errors).
valid_elements(SSL, relation, Name, [QualName|QualNames], [Err|Errors]) :-
    format(atom(Err), 'Invalid relation ~w reference to ~w', [ Name, QualName ]),
    valid_elements(SSL, relation, Name, QualNames, Errors).

% Judgement 24: Valid elements of a System
valid_elements(_SSL, system, _, [], []).
valid_elements(SSL, system, Name, [E|ES], Errors) :-
    specElement_type(E, EType),
    member(EType, ["Subsystem", "Imported Subsystem", "Relation" ]), !,
    valid_elements(SSL, system, Name, ES, Errors).
valid_elements(SSL, system, Name, [E|ES], [Err|Errors]) :-
    specElement_type(E, EType),
    format(atom(Err), "Invalid element for System ~w: ~w", [ Name, EType ]),
    valid_elements(SSL, system, Name, ES, Errors).
% Judgement 25 and 26: Valid elements of a System
valid_elements(_SSL, subsystem, _, [], []).
valid_elements(SSL, subsystem, Name, [E|ES], Errors) :-
    specElement_type(E, EType),
    member(EType, ["Subsystem", "Imported Subsystem",
                   "Component", "Imported Component",
                   "Scenarios", "Requirements", "Events", "Relations"
                  ]), !,
    valid_elements(SSL, system, Name, ES, Errors).
valid_elements(SSL, subsystem, Name, [E|ES], [Err|Errors]) :-
    specElement_type(E, EType),
    format(atom(Err), "Invalid element for SubSystem ~w: ~w", [ Name, EType ]),
    valid_elements(SSL, system, Name, ES, Errors).
