:- module(englib, [ enumerate/2,
                    append_nub/3,
                    intercalate/3,
                    list_call/3,
                    show_type/2,
                    string_trim/2,
                    string_codes_ltrim/3,
                    string_codes_rtrim/3,
                    string_rpad/4,
                    string_contains/2,
                    subst/4,
                    get_dict_or/4,
                    ensure_dir/1,
                    ensure_file_loc/1,
                    format_lines/2,
                    write_strings/2,
                    %% classification_marks/1,
                    assert_eng/2,
                    assert_eng/3
                  ]).

:- use_module(library(apply)).
:- use_module(library(lists)).

list_call(Op, [X|XS], Out) :- list_call(call(Op, X), XS, Out).
list_call(Op, [], Out) :- call(Op, Out).

enumerate(I, O) :- enum_(0, I, O).
enum_(_, [], []).
enum_(N, [I|IS], [(N,I)|OS]) :- succ(N, M), enum_(M, IS, OS).

% like append, but does not add duplicate entries, right biased
append_nub([], ES, ES).
append_nub([N|NS], ES, R) :-
    append_nub(NS, ES, OS), (member(N, OS) -> R = OS ; R = [N|OS]).

intercalate([], _, "").
intercalate([E], _, E) :- string(E), !.
intercalate([E], _, ES) :- atom(E), !, atom_string(E, ES).
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
show_type(X, "number") :- number(X), !.
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

% Substitute all occurrences of This with That in Inp list.  From
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

% True if the second string argument is contained within the first
string_contains(Str, Part) :-
    string_concat(_, Mid, Str),
    string_concat(Part, _, Mid), !.


get_dict_or(Key, Dict, _, Val) :- get_dict(Key, Dict, Val), !.
get_dict_or(_, _, Def, Def).

format_lines(_, []).
format_lines(Fmt, [E|ES]) :-
    format(Fmt, E),
    format_lines(Fmt, ES).

write_strings(Indent, Lines) :-
    maplist(string_concat(Indent), Lines, IndLines),
    string_lines(Str, IndLines),
    writeln(Str).


% ensure_file_loc ensures that the directory for the specified file path exists.
ensure_file_loc(FilePath) :-
    directory_file_path(Dir, _, FilePath),
    ensure_dir(Dir).

% ensure_dir ensures that the specified directory exists.
ensure_dir(Dir) :- directory_file_path(Top, Top, Dir), !.
ensure_dir(Dir) :- directory_file_path(Parent, _, Dir),
                   ensure_dir(Parent),
                   (exists_directory(Dir), !; make_directory(Dir)).



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
assert_eng([K1], [Ref]) :-
    atom_string(K1a, K1),
    assertz(eng:key(K1a), Ref).
assert_eng([K1,K2], [Ref]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    assertz(eng:key(K1a,K2a), Ref).
assert_eng([K1,K2,K3], [Ref]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    assertz(eng:key(K1a,K2a,K3a), Ref).
assert_eng([K1,K2,K3,K4], [Ref]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    assertz(eng:key(K1a,K2a,K3a,K4a), Ref).
assert_eng([K1,K2,K3,K4,K5], [Ref]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a), Ref).
assert_eng([K1,K2,K3,K4,K5,K6], [Ref]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a,K6a), Ref).
assert_eng([K1,K2,K3,K4,K5,K6,K7], [Ref]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a,K6a,K7a), Ref).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8], [Ref]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    atom_string(K8a, K8),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a,K6a,K7a,K8a), Ref).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8,K9], [Ref]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    atom_string(K8a, K8),
    atom_string(K9a, K9),
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a,K6a,K7a,K8a,K9a), Ref).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8,K9,K10], [Ref]) :-
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
    assertz(eng:key(K1a,K2a,K3a,K4a,K5a,K6a,K7a,K8a,K9a,K10a), Ref).

assert_val(X, X) :- \+ is_list(X).

assert_eng([K1], VS, [KeyRef, ValRef]) :-
    atom_string(K1a, K1),
    assert_val(VS, VA),
    assertz(eng:key(K1a), KeyRef),
    assertz(eng:eng(K1a, VA), ValRef).
assert_eng([K1,K2], VS, [KeyRef, ValRef]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a), KeyRef),
    assertz(eng:eng(K1a, K2a, VA), ValRef).
assert_eng([K1,K2,K3],VS, [KeyRef, ValRef]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a), KeyRef),
    assertz(eng:eng(K1a, K2a, K3a, VA), ValRef).
assert_eng([K1,K2,K3,K4],VS, [KeyRef, ValRef]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a), KeyRef),
    assertz(eng:eng(K1a, K2a, K3a, K4a, VA), ValRef).
assert_eng([K1,K2,K3,K4,K5],VS, [KeyRef, ValRef]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a), KeyRef),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, VA), ValRef).
assert_eng([K1,K2,K3,K4,K5,K6],VS, [KeyRef, ValRef]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a, K6a), KeyRef),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, K6a, VA), ValRef).
assert_eng([K1,K2,K3,K4,K5,K6,K7],VS, [KeyRef, ValRef]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a, K6a, K7a), KeyRef),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, K6a, K7a, VA), ValRef).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8],VS, [KeyRef, ValRef]) :-
    atom_string(K1a, K1),
    atom_string(K2a, K2),
    atom_string(K3a, K3),
    atom_string(K4a, K4),
    atom_string(K5a, K5),
    atom_string(K6a, K6),
    atom_string(K7a, K7),
    atom_string(K8a, K8),
    assert_val(VS, VA),
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a), KeyRef),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a, VA), ValRef).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8,K9],VS, [KeyRef, ValRef]) :-
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
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a, K9a), KeyRef),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a, K9a, VA), ValRef).
assert_eng([K1,K2,K3,K4,K5,K6,K7,K8,K9,K10],VS, [KeyRef, ValRef]) :-
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
    assertz(eng:key(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a, K9a, K10a), KeyRef),
    assertz(eng:eng(K1a, K2a, K3a, K4a, K5a, K6a, K7a, K8a, K9a, K10a, VA), ValRef).
