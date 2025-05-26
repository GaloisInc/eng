:- use_module(library(http/json)).

compare() :-
    current_prolog_flag(argv, [OldFile, NewFile]),
    open(OldFile, read, OF),
    open(NewFile, read, NF),
    !,
    json_read_dict(OF, OJ),
    json_read_dict(NF, NJ),
    !,
    compare1(OJ, NJ).

compare() :-
    format('USAGE: cmp_fret_json OLDFILE.json NEWFILE.json~n').

compare2(OJ, NJ) :-
    % Try just rewriting the FRET generated file to see if swi prolog will
    % normalize [update: it won't].  If it doesn't, fall through to compare1.
    open('cmp.json', write, CF),
    json_write_dict(CF, NJ),
    close(CF),
    open('cmp.json', read, CF2),
    json_read_dict(CF2, NJ2),
    (OJ == NJ2
    -> writeln(same_same)
    ;  compare1(OJ, NJ2)
    ).

compare1(OJ, NJ) :-
    % Easy check: they are the same, otherwise, compare by element.
    (OJ == NJ
    -> writeln('the files are identical')
    ; cmp_json([], OJ, NJ)
    ).

% cmp_json will compare objects (dictionaries) by field name, and lists by index,
% except for the top-level "requirements" and "variables" arrays; the order of
% these are not stable as emitted by FRET, but each element is a dict, so compare
% elements on their "_id" entry.
%
% Note that every comparison ends in fail to force backtracking and trying all
% object fields/list elements.
cmp_json(Lvl, OJ, NJ) :-
    is_dict(OJ),
    is_dict(NJ),
    !,
    get_dict(F, OJ, OF),
    show_keys(Lvl, F),
    get_dict(F, NJ, NF),
    NLvl = [F|Lvl],
    cmp_json(NLvl, OF, NF).
cmp_json([variables], OL, NL) :-
    is_list(OL),
    is_list(NL),
    !,
    cmp_list_on([0,variables], field_id, OL, NL).
cmp_json([requirements], OL, NL) :-
    is_list(OL),
    is_list(NL),
    !,
    cmp_list_on([0,requirements], field_id, OL, NL).
cmp_json([L|Lvl], OL, NL) :-
    is_list(OL),
    is_list(NL),
    !,
    cmp_list([0,L|Lvl], OL, NL).
cmp_json(_, O, O) :- !, fail.
cmp_json(Lvl, O, N) :-
    reverse(Lvl, Keys),
    atomics_to_string(Keys, '.', Key),
    format('DIFF @ ~w::~n   old: ~w~n   new: ~w~n~n', [Key, O, N]),
    fail.

cmp_list(_, [], []) :- !, fail.
cmp_list(_, [O|OS], []) :- format('OldELEMs: ~w~n', [O|OS]).
cmp_list(_, [], [N|NS]) :- format('NewELEMs: ~w~n', [N|NS]).
cmp_list([I|Lvl], [O|OS], [N|NS]) :-
    show_keys(Lvl, I),
    (cmp_json([I|Lvl], O, N), !; true),
    succ(I, J),
    cmp_list([J|Lvl], OS, NS).

cmp_list_on(_, _, [], []) :- !, fail.
cmp_list_on(_, _, [O|OS], []) :- format('OldELEMs: ~w~n', [O|OS]).
cmp_list_on(_, _, [], [N|NS]) :- format('NewELEMs: ~w~n', [N|NS]).
cmp_list_on([I|Lvl], Select_Right, [O|OS], Rights) :-
    call(Select_Right, O, Rights, T, N, NS),
    format(atom(K), '~w:~w', [I, T]),
    show_keys(Lvl, K),
    (cmp_json([K|Lvl], O, N), !; true),
    succ(I, J),
    !,
    cmp_list_on([J|Lvl], Select_Right, OS, NS).

% Enable the second form for tracing
show_keys(_Lvl, _K).
%% show_keys(Lvl, K) :-
%%     length(Lvl, ILvl),
%%     format(atom(Fmt), '~~`.t~~~w| ~~w~~n', [ILvl]),
%%     format(Fmt, [K]).

field_id(Left, [R|Rights], ID, R, Rights) :-
    get_dict('_id', Left, ID),
    get_dict('_id', R, ID),
    !.
field_id(Left, [R|Rights], ID, Right, [R|Others]) :-
    field_id(Left, Rights, ID, Right, Others).
