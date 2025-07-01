:- begin_tests(exprlang).
:- use_module(exprlang).
:- use_module(library(strings)).

rtpe(LangDef, Inp) :-
    parse_expr(LangDef, Inp, ABT),
    emit_expr(LangDef, ABT, Out),
    split_string(Inp, "", " ", [InpTrimmed]),
    assertion(InpTrimmed == Out).

rtpe(LangDef, Inp, ExpABT) :-
    parse_expr(LangDef, Inp, ABT),
    assertion(ExpABT = ABT),
    emit_expr(LangDef, ABT, Out),
    split_string(Inp, "", " ", [InpTrimmed]),
    assertion(InpTrimmed == Out).


langdef1(
    langdef{
        phrases:
        [ term(number, num, [term(N, number), T]>>fmt_str(T, '~w', [N])),
          term(bool, [true]>>word(true), emit_simple_term),
          term(bool, [false]>>word(false), emit_simple_term),
          term(ident, word, emit_simple_term),
          expop(add ⦂ num → num → num, infix(tok('+')), emit_infix("+")),
          expop(sub ⦂ num → num → num, infix(tok('-')), emit_infix("-")),
          expop(cmpeq ⦂ a → a → bool, infix(chrs('==')), emit_infix("=="))
        ]}).

%% --------------------

test(empty, [nondet]) :- rtpe(_, "").

test(no_langdef, [nondet, fail]) :- rtpe(langdef{terms:[]}, "32").

test(num_term, [nondet]) :- langdef1(LangDef1), rtpe(LangDef1, "32").

test(space_num_term, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "   3278234998", term(3278234998, number)).

test(word_term, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "hello_f1rst").

test(space_word_term, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "    hello_f1rst").

test(true_term, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "    true", term(true, bool)).

test(infix_expr_term, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "hello_f1rst", term("hello_f1rst", ident)).

test(infix_expr_terms, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 + 54", add(term(32, number), term(54, number))).

test(infix_expr_nested, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 + 19 - 54 + 87",
         add(term(32, number),
             sub(term(19, number),
                 add(term(54, number), term(87, number))))).

test(infix_expr_bool, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 == 19", cmpeq(term(32, number), term(19, number))).

test(infix_expr_bool_badtypes, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 == false", cmpeq(term(32, number), term(false, bool))).

test(infix_expr_bool_ident, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 == some_num",
         cmpeq(term(32, number), term("some_num", ident))).
