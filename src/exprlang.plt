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

langdef1(langdef{
             terms:
             [ term(number, num, [term(N, number), T]>>fmt_str(T, '~w', [N])),
               term(ident, word, [term(N, ident), T]>>fmt_str(T, '~w', [N])),
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
