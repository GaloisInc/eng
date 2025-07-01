:- begin_tests(exprlang).
:- use_module(exprlang).
:- use_module(library(strings)).

rtpe(LangDef, Inp) :-
    parse_expr(LangDef, Inp, ABT),
    !,
    emit_expr(LangDef, ABT, Out),
    split_string(Inp, "", " ", [InpTrimmed]),
    assertion(InpTrimmed == Out).

rtpe(LangDef, Inp, ExpABT) :-
    parse_expr(LangDef, Inp, ABT),
    !,
    assertion(ExpABT = ABT),
    emit_expr(LangDef, ABT, Out),
    split_string(Inp, "", " ", [InpTrimmed]),
    assertion(InpTrimmed == Out).

rtpe(LangDef, Inp, ExpABT, ExpOut) :-
    parse_expr(LangDef, Inp, ABT),
    !,
    assertion(ExpABT = ABT),
    emit_expr(LangDef, ABT, Out),
    assertion(ExpOut = Out).


langdef1(
    langdef{
        types: [ number, bool ],
        atoms: [ lit, num ],
        phrases:
        [ term(num ⦂ number, num, [term(num(N), number), T]>>fmt_str(T, '~w', [N])),
          term(lit ⦂ bool, [true]>>word(true), emit_simple_term(lit)),
          term(lit ⦂ bool, [false]>>word(false), emit_simple_term(lit)),
          term(ident ⦂ a, word, emit_simple_term(ident)),
          expop(add ⦂ number → number → number, infix(tok('+')), emit_infix("+")),
          expop(sub ⦂ number → number → number, infix(tok('-')), emit_infix("-")),
          expop(cmpeq ⦂ a → a → bool, infix(chrs('==')), emit_infix("==")),
          expop(const ⦂ a → b → a, [[]>>word(const),
                                    []>>chrs('('),
                                    subexpr,
                                    []>>lexeme(chrs(',')),
                                    subexpr,
                                    []>>lexeme(chrs(')'))
                                   ],
                [F,A,B,T]>>fmt_str(T, '~w(~w, ~w)', [F, A, B]))
        ]}).


%% --------------------

test(empty, [nondet]) :- rtpe(langdef{phrases:[]}, "").

test(no_langdef, [nondet, fail]) :- rtpe(langdef{phrases:[]}, "32").

test(num_term, [nondet]) :- langdef1(LangDef1), rtpe(LangDef1, "32").

test(space_num_term, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "   3278234998", term(num(3278234998), number)).

test(word_term, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "hello_f1rst").

test(space_word_term, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "    hello_f1rst").

test(true_term, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "    true", term(lit(true), bool)).

test(infix_expr_term, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "hello_f1rst", term(ident("hello_f1rst"), type_unassigned)).

test(infix_expr_terms, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 + 54",
         term(add(term(num(32), number), term(num(54), number)), number),
        "(32 + 54)").

test(infix_expr_terms_paren, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "(32 + 54)",
         term(add(term(num(32), number), term(num(54), number)), number)).

test(infix_expr_nested, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 + 19 - 54 + 87",
         term(add(term(num(32), number),
                  term(sub(term(num(19), number),
                           term(add(term(num(54), number), term(num(87), number)),
                                number)),
                       number)),
              number),
        "(32 + (19 - (54 + 87)))").

test(infix_expr_nested_parens, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 + (19 - 54) + 87",
         term(add(term(num(32), number),
                  term(add(term(sub(term(num(19), number),
                                    term(num(54), number)), number),
                           term(num(87), number)),
                       number)),
              number),
        "(32 + ((19 - 54) + 87))").

test(infix_expr_bool, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 == 19",
         term(cmpeq(term(num(32), number), term(num(19), number)), bool),
        "(32 == 19)").

test(infix_expr_bool_badtypes, [nondet, fail]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 == false", cmpeq(term(num(32), number),
                                        term(lit(false), bool))).

test(infix_expr_bool_ident, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 == some_num",
         term(cmpeq(term(num(32), number), term(ident("some_num"), number)),
              bool),
        "(32 == some_num)").

test(multiarg_non_static_types, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(32, some_num)",
         term(const(term(num(32), number), term(ident("some_num"), type_unassigned)),
              number)).
