:- module(englib, [ intercalate/3,
                    show_type/2,
                    string_trim/2,
                    string_codes_ltrim/3,
                    string_codes_rtrim/3,
                    string_rpad/4,
                    subst/4,
                    get_dict_or/4,
                    write_strings/2,
                    assert_eng/1,
                    assert_eng/2
                  ]).

:- use_module(library(apply)).
:- use_module(library(lists)).

intercalate([], _, "").
intercalate([E], _, E) :- string(E).
intercalate([E], _, ES) :- atom(E), atom_string(E, ES).
intercalate([E|ES], S, R) :- intercalate(ES, S, RS),
                             ( string(E), !, string_concat(E, S, RE)
                             ; atom(E), !, (atom_string(E, SE),
                                            string_concat(SE, S, RE))
                             ; print_message(error, unknown_type(intercalate,E)),
                               RE = S
                             ),
                             string_concat(RE, RS, R).

prolog:message(unknown_type(Where, What)) -->
    [ 'Unknown type provided to ~w: ~w~n' - [ Where, What ] ].

show_type(X, "atom") :- atom(X), !.
show_type(X, "string") :- string(X), !.
show_type(X, "var") :- var(X), !.
show_type(X, R) :- is_list(X), !,
                   maplist(show_type, X, XTS),
                   intercalate(XTS, ", ", XTSS),
                   string_concat("list: ", XTSS, R).
show_type(X, "atomic") :- atomic(X), !.
show_type(X, "compound") :- compound(X), writeln(X), !.
show_type(_, "other").

string_trim(IS, OS) :-
    split_string(IS, "", "\t\s\n", [OS]).
    %% string_codes(IS, IC),
    %% string_codes_rtrim(IC, MC, _),
    %% string_codes_ltrim(MC, OC, _),
    %% string_codes(OS, OC).

% Remove spaces from the right of the IS string codes list, returning the
% trimmed string codes, and the number of removed spaces.
string_codes_ltrim([I|IS], OS, TC) :- char_type(I, space), !,
                                      string_codes_ltrim(IS, OS, NTC),
                                      succ(NTC, TC).
string_codes_ltrim(IS, IS, 0).

% Remove spaces from the right of the IS string codes list, returning the
% trimmed string codes, and the number of removed spaces.
string_codes_rtrim(IS, OS, TC) :-
    append(IS_, [L], IS), char_type(L, space), !,
    string_codes_rtrim(IS_, OS, OC),
    succ(OC, TC).
string_codes_rtrim(IS, IS, 0) :- append(_, [L], IS), \+ char_type(L, space).
string_codes_rtrim([], [], 0).

% Adds N count of P to end (right) of S
string_rpad(S, _, 0, S).
string_rpad(S, P, N, OS) :-
    N > 0,
    succ(D, N),
    string_rpad(S, P, D, SS),
    string_concat(SS, P, OS).

% Substitue all occurrences of This with That in Inp list.  From
% https://github.com/SWI-Prolog/plweb-examples/blob/master/usage/substitute-with-append.md
substl(This, That, Inp, Out) :-
    append(This, After, Rest),
    append(Before, Rest, Inp),
    !,
    substl(This, That, After, AfterResult),
    append([Before, That, AfterResult], Out).
substl(_, _, S, S).

subst(ThisStr, ThatStr, InpStr, OutStr) :-
    string_chars(ThisStr, This),
    string_chars(ThatStr, That),
    string_chars(InpStr, Inp),
    substl(This, That, Inp, Out),
    string_chars(OutStr, Out).

get_dict_or(Key, Dict, _, Val) :- get_dict(Key, Dict, Val), !.
get_dict_or(_, _, Def, Def).

write_strings(Indent, Lines) :-
    maplist(string_concat(Indent), Lines, IndLines),
    string_lines(Str, IndLines),
    writeln(Str).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic eng:key/1.
:- dynamic eng:key/2.
:- dynamic eng:key/3.
:- dynamic eng:key/4.
:- dynamic eng:key/5.
:- dynamic eng:key/6.
:- dynamic eng:key/7.
:- dynamic eng:key/8.
:- dynamic eng:key/9.
:- dynamic eng:key/10.

:- dynamic eng:eng/1.
:- dynamic eng:eng/2.
:- dynamic eng:eng/3.
:- dynamic eng:eng/4.
:- dynamic eng:eng/5.
:- dynamic eng:eng/6.
:- dynamic eng:eng/7.
:- dynamic eng:eng/8.
:- dynamic eng:eng/9.
:- dynamic eng:eng/10.
:- dynamic eng:eng/11.


% Assert a fact with no value.  The replication here is ugly, but there's no good
% way to dynamically perform these.
assert_eng([K1]) :-
    atom_string(K1a, K1),
    assertz(eng:key(K1a)).
assert_eng([K1,K2]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    assertz(eng:key(K1a,K2a)).
assert_eng([K1,K2,K3]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    assertz(eng:key(K1a,K2a,K3a)).
assert_eng([K1,K2,K3,K4]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    assertz(eng:key(K1a,K2a,K3a,K4a)).
assert_eng([K1,K2,K3,K4,K5]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a)).
assert_eng([K1,K2,K3,K4,K5,K6]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a,K6a)).
assert_eng([K1,K2,K3,K4,K5,K6,K7]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a,K6a,K7a)).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    atom_string(K8a, K8),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a,K6a,K7a,K8a)).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8,K9]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    atom_string(K8a, K8),
    atom_string(K9a, K9),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a,K6a,K7a,K8a,K9a)).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8,K9,K10]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    atom_string(K8a, K8),
    atom_string(K9a, K9),
    atom_string(K10a, K10),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a,K6a,K7a,K8a,K9a,K10a)).

assert_val(X, X) :- \+ is_list(X).

assert_eng([K1], VS) :-
    atom_string(K1a, K1),
    assert_val(VS, VA),
    assertz(eng:key(K1a)),
    assertz(eng:eng(K1a, VA)).
assert_eng([K1,K2], VS) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a)),
    assertz(eng:eng(K1a, K2a, VA)).
assert_eng([K1,K2,K3],VS) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a)),
    assertz(eng:eng(K1a, K2a, K3a, VA)).
assert_eng([K1,K2,K3,K4],VS) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a)),
    assertz(eng:eng(K1a, K2a, K3a, K4a, VA)).
assert_eng([K1,K2,K3,K4,K5],VS) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a)),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, VA)).
assert_eng([K1,K2,K3,K4,K5,K6],VS) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a, K6a)),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, K6a, VA)).
assert_eng([K1,K2,K3,K4,K5,K6,K7],VS) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a, K6a, K7a)),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, K6a, K7a, VA)).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8],VS) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    atom_string(K8a, K8),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a)),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a, VA)).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8,K9],VS) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    atom_string(K8a, K8),
    atom_string(K9a, K9),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a, K9a)),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a, K9a, VA)).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8,K9,K10],VS) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    atom_string(K8a, K8),
    atom_string(K9a, K9),
    atom_string(K10a, K10),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a, K9a, K10a)),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a, K9a, K10a, VA)).
