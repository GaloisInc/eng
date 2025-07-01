:- begin_tests(exprlang).
:- use_module(exprlang).
:- use_module(library(strings)).

rtpe(LangDef, Inp) :-
    parse_expr(LangDef, Inp, ABT),
    !,
    get_dict(language, LangDef, Language),
    emit_expr(Language, ABT, Out),
    split_string(Inp, "", " ", [InpTrimmed]),
    assertion(InpTrimmed == Out).

rtpe(LangDef, Inp, ExpABT) :-
    parse_expr(LangDef, Inp, ABT),
    !,
    assertion(ExpABT = ABT),
    get_dict(language, LangDef, Language),
    emit_expr(Language, ABT, Out),
    split_string(Inp, "", " ", [InpTrimmed]),
    assertion(InpTrimmed == Out).

rtpe(LangDef, Inp, ExpABT, ExpOut) :-
    parse_expr(LangDef, Inp, ABT),
    !,
    assertion(ExpABT = ABT),
    get_dict(language, LangDef, Language),
    emit_expr(Language, ABT, Out),
    assertion(ExpOut = Out).


langdef1(
    langdef{
        language: testlang,
        types: [ number, bool ],
        atoms: [ lit, num ],
        variable_ref: [ ident ],
        phrases:
        [ term(num ⦂ number, num, [term(num(N), number), T]>>fmt_str(T, '~w', [N])),
          term(lit ⦂ bool, [true]>>word(true), emit_simple_term(lit)),
          term(lit ⦂ bool, [false]>>word(false), emit_simple_term(lit)),
          term(ident ⦂ a, word, emit_simple_term(ident)),
          expop(add ⦂ number → number → number, infix(chrs('+')), emit_infix("+")),
          expop(sub ⦂ number → number → number, infix(chrs('-')), emit_infix("-")),
          expop(cmpeq ⦂ a → a → bool, infix(chrs('==')), emit_infix("==")),
          expop(const ⦂ a → b → a, [[]>>word(const),
                                    []>>chrs('('),
                                    subexpr,
                                    []>>lexeme(chrs(',')),
                                    subexpr,
                                    []>>lexeme(chrs(')'))
                                   ],
                [F,[A,B],T]>>fmt_str(T, '~w(~w, ~w)', [F, A, B]))
        ]}).


%% --------------------

test(empty, [nondet]) :- rtpe(langdef{language:t1,
                                      types:[],
                                      atoms:[],
                                      variable_ref:[],
                                      phrases:[]}, "").

test(no_langdef, [nondet, fail]) :- rtpe(langdef{language:t2,
                                                 types:[],
                                                 atoms:[],
                                                 variable_ref:[],
                                                 phrases:[]}, "32").

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
    rtpe(LangDef1, "hello_f1rst", term(ident("hello_f1rst"), type_unassigned('⚲T0'))).

test(infix_expr_terms, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 + 54",
         op(add(term(num(32), number), term(num(54), number)), number),
        "(32 + 54)").

test(infix_expr_terms_paren, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "(32 + 54)",
         op(add(term(num(32), number), term(num(54), number)), number)).

test(infix_expr_nested, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 + 19 - 54 + 87",
         op(add(term(num(32), number),
                op(sub(term(num(19), number),
                       op(add(term(num(54), number), term(num(87), number)),
                          number)),
                   number)),
            number),
        "(32 + (19 - (54 + 87)))").

test(infix_expr_nested_parens, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 + (19 - 54) + 87",
         op(add(term(num(32), number),
                op(add(op(sub(term(num(19), number),
                              term(num(54), number)), number),
                       term(num(87), number)),
                   number)),
            number),
         "(32 + ((19 - 54) + 87))").

test(infix_expr_bool, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 == 19",
         op(cmpeq(term(num(32), number), term(num(19), number)), bool),
        "(32 == 19)").

test(infix_expr_bool_badtypes, [nondet, fail]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 == false", cmpeq(term(num(32), number),
                                        term(lit(false), bool))).

test(infix_expr_bool_ident, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "32 == some_num",
         op(cmpeq(term(num(32), number), term(ident("some_num"), number)), bool),
        "(32 == some_num)").

test(multiarg_non_static_types, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(32, some_num)",
         op(const(term(num(32), number),
                  term(ident("some_num"), type_unassigned('⚲T0'))),
              number)).

test(multiarg_non_static_types_expr, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(32 + 54, some_num)",
         op(const(op(add(term(num(32), number), term(num(54), number)), number),
                  term(ident("some_num"), type_unassigned('⚲T0'))),
            number),
        "const((32 + 54), some_num)").

test(multiarg_non_static_types_complex_expr, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(32+ ( (54-19)-6 ), const(3, some_num == some_num))",
         op(const(op(add(term(num(32), number),
                         op(sub(op(sub(term(num(54), number),
                                       term(num(19), number)), number),
                                term(num(6), number)), number)), number),
                  op(const(term(num(3), number),
                           op(cmpeq(term(ident("some_num"), type_unassigned('⚲T0')),
                                    term(ident("some_num"), type_unassigned('⚲T0'))),
                              bool)),
                     number)),
            number),
        "const((32 + ((54 - 19) - 6)), const(3, (some_num == some_num)))").

test(indeterminate_types, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(some_num, true)",
         op(const(term(ident("some_num"), type_unassigned('⚲T0')),
                  term(lit(true), bool)),
            type_unassigned('⚲T0')),
         "const(some_num, true)").

test(whitespace, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "    const(   some_num   ,    true   )   ",
         op(const(term(ident("some_num"), type_unassigned('⚲T0')),
                  term(lit(true), bool)),
            type_unassigned('⚲T0')),
         "const(some_num, true)").

test(result_determines_arg_types, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(some_num, true) == const(other_thing, 13)",
         op(cmpeq(op(const(term(ident("some_num"), type_unassigned('⚲T0')),
                           term(lit(true), bool)),
                     type_unassigned('⚲T0')),
                  op(const(term(ident("other_thing"), type_unassigned('⚲T0')),
                           term(num(13), number)),
                     type_unassigned('⚲T0'))),
            bool),
         "(const(some_num, true) == const(other_thing, 13))").

test(complex_nesting_indeterminate_types, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "   const(some_num, true == const(foo, 32+const(19-38, unruly)))
                    == const(const(other_thing, this_thing), 13)",
         op(cmpeq(op(const(term(ident("some_num"), type_unassigned('⚲T0')),
                           op(cmpeq(term(lit(true), bool),
                                    op(const(term(ident("foo"), bool),
                                             op(add(term(num(32), number),
                                                    op(const(op(sub(term(num(19), number),
                                                                    term(num(38), number)),
                                                                number),
                                                             term(ident("unruly"),
                                                                  type_unassigned('⚲T2'))),
                                                      number)),
                                                number)),
                                       bool)),
                              bool)),
                     type_unassigned('⚲T0')) ,
                  op(const(op(const(term(ident("other_thing"), type_unassigned('⚲T0')),
                           term(ident("this_thing"), type_unassigned('⚲T4'))),
                              type_unassigned('⚲T0')),
                           term(num(13), number)),
                     type_unassigned('⚲T0'))),
            bool),
         "(const(some_num, (true == const(foo, (32 + const((19 - 38), unruly))))) == const(const(other_thing, this_thing), 13))").

test(fixed_vars, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "some_num + other_thing",
         op(add(term(ident("some_num"), number),
                term(ident("other_thing"), number)), number),
         "(some_num + other_thing)").

test(align_terms_result, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(other_thing, other_thing == true)",
         op(const(term(ident("other_thing"), bool),
                  op(cmpeq(term(ident("other_thing"), bool),
                           term(lit(true), bool)),
                     bool)),
            bool),
         "const(other_thing, (other_thing == true))").

test(fixed_incompat_vars, [nondet, fail]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "some_num + const(other_thing, other_thing == true)",
         op(other_thing, bool),
         "other_thing is a bool, but add requires a number").

test(fixed_incompat_vars_reverse, [nondet, fail]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(other_thing, other_thing == true) + some_num",
         op(other_thing, bool),
         "other_thing is a bool, but add requires a number").
