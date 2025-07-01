:- module(exprlang, [ parse_expr/3,
                      op(900, xfy, →),
                      op(910, yfx, ⦂),
                      emit_expr/3, emit_simple_term/2, emit_infix/5,
                      % Helpers
                      num/3, word/3, lexeme/3, tok/3, chrs/3,
                      fmt_str/3
                    ]).

:- use_module(library(yall)).


parse_expr(LangDef, Expr, ABT) :-
    string_chars(Expr, ECodes),
    enumerate(ECodes, Input),
    phrase(expr(LangDef, ABT), Input).

expr(_, end) --> [].
expr(LangDef, Expr) -->
    { get_dict(phrases, LangDef, LangPhrases),
      member(term(TermType, TermParser, _), LangPhrases)
    },
    lexeme(call(TermParser, P)),
    exprMore(LangDef, term(P, TermType), Expr).

exprMore(LangDef, LeftTerm, Expr) -->
    { get_dict(phrases, LangDef, LangPhrases),
      member(expop(Op ⦂ OpType, infix(OpParser), _), LangPhrases)
    },
    lexeme(call(OpParser)),
    lexeme(expr(LangDef, RT)),
    { Expr =.. [Op, LeftTerm, RT] }.
exprMore(LangDef, E, E) --> [].
exprMore(_, E, _) -->
    any(20, V, P),
    { print_message(error, invalid_expr(E, V, P)), !, fail }.

prolog:message(invalid_expr(E, V, P)) -->
    [ 'Expected more of expression "~w" @ offset ~w: ~w' - [E, P, V]].

%% ----------------------------------------------------------------------

emit_expr(LangDef, term(P, TermType), Expr) :-
    get_dict(phrases, LangDef, LangPhrases),
    member(term(TermType, _, TermEmitter), LangPhrases),
    call(TermEmitter, term(P, TermType), Expr).
emit_expr(LangDef, Op, Expr) :-
    Op =.. [ BinOp, Arg1, Arg2 ],
    get_dict(phrases, LangDef, LangPhrases),
    member(expop(BinOp ⦂ _, _, TermEmitter), LangPhrases),
    emit_expr(LangDef, Arg1, A),
    emit_expr(LangDef, Arg2, B),
    call(TermEmitter, BinOp, A, B, Expr).
emit_expr(_, end, "") :- !.
emit_expr(_, ABT, Expr) :- fmt_str(Expr, '<<~w>>', [ABT]).

emit_simple_term(term(A, _), T) :- fmt_str(T, '~w', A).
emit_infix(Repr, _, LA, RA, T) :- fmt_str(T, '~w ~w ~w', [ LA, Repr, RA ]).

%% ----------------------------------------------------------------------
%% Parsing Helpers

enumerate(I, O) :- enum_(0, I, O).
enum_(_, [], []).
enum_(N, [I|IS], [(N,I)|OS]) :- succ(N, M), enum_(M, IS, OS).

fmt_str(V, Fmt, Args) :- format(atom(A), Fmt, Args), atom_string(A, V).

any(N, S, P) --> any_(N, L, P), {string_codes(S, L)}.
any_(N, [C|CS], P) --> [(P,C)], {succ(M, N)}, any_(M, CS, _).
any_(0, [], span(99999,99999)) --> [].
any_(_, [], span(99998,99998)) --> [].

word(W) --> [(_N,C)], { word_char(C), \+ char_type(C, digit) },
            wc(CS), { string_codes(W, [C|CS]) }.
wc([C|CS]) --> [(_N,C)], { word_char(C) }, !, wc(CS).
wc([]) --> [].

word_char(C) :- \+ char_type(C, space),
                % Exclude things that might be individual tokens needing to be
                % recognized elsewhere in the grammar.
                \+ member(C, ['(', ')', '.', '!', '?', ':', ',',
                              %% '{', '}', '^', '[', ']', %% XXX?
                              '$',
                              '=',
                              '/']).

num(V) --> num_(Digits), { to_num(Digits, 0, V) }.
num_([N|NS]) --> dig(N, _), num_(NS).
num_([N]) --> dig(N, _).

to_num([], A, A).
to_num([D|DS], A, V) :- N is A * 10 + D, to_num(DS, N, V).

dig(D, span(P, P)) --> [ (P,C) ], { char_type(C, digit),
                                    atom_codes(C, [CS]),
                                    atom_codes('0', [ZS]),
                                    plus(ZS, D, CS)
                                  }.

lexeme(R) --> ws(_), !, lexeme(R).
lexeme(R) --> call(R).

tok(M) --> word(W), { any_case_match([A], W), atom_string(A,M) }.

any_case_match(Candidates, Word) :- to_lower(Word, LCWord),
                                    member(LCWord, Candidates).

to_lower(I, O) :- atom_string(IA, I), downcase_atom(IA, OA), atom_string(OA, O).

ws(span(N,N)) --> [(N,C)], { char_type(C, space) }.

chrs(Chars) --> { atom_chars(Chars, [C|CS]) }, [(_,C)], chrs_(CS).
chrs_([C|CS]) --> [(_,C)], chrs_(CS).
chrs_([]) --> [].
