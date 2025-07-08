:- encoding(utf8).
:- begin_tests(exprlang).
:- use_module(exprlang).
:- use_module(library(strings)).

rtpe(LangDef, Inp) :-
    define_language(LangDef, _),
    get_dict(language, LangDef, Language),
    parse_expr(Language, Inp, ABT),
    !,
    emit_expr(Language, ABT, Out),
    split_string(Inp, "", " ", [InpTrimmed]),
    assertion(InpTrimmed == Out),
    % above may have normalized: ensure it is still parseable
    parse_expr(Language, Out, ABT2),
    !,
    assertion(ABT == ABT2),
    emit_expr(Language, ABT2, Out2),
    assertion(InpTrimmed == Out2).

rtpe(LangDef, Inp, ExpABT) :-
    define_language(LangDef, _),
    get_dict(language, LangDef, Language),
    parse_expr(Language, Inp, ABT),
    !,
    assertion(ExpABT = ABT),
    emit_expr(Language, ABT, Out),
    split_string(Inp, "", " ", [InpTrimmed]),
    assertion(InpTrimmed == Out),
    % above may have normalized: ensure it is still parseable
    parse_expr(Language, Out, ABT2),
    !,
    assertion(ABT == ABT2),
    emit_expr(Language, ABT2, Out2),
    assertion(InpTrimmed == Out2).

rtpe(LangDef, Inp, ExpABT, ExpOut, ExpType) :-
    define_language(LangDef, _),
    get_dict(language, LangDef, Language),
    parse_expr(Language, Inp, ABT, ExpType),
    !,
    assertion(ExpABT = ABT),
    emit_expr(Language, ABT, Out),
    assertion(ExpOut = Out),
    % above may have normalized: ensure it is still parseable
    parse_expr(Language, Out, ABT2, ExpType),
    !,
    assertion(ABT == ABT2),
    emit_expr(Language, ABT2, Out2),
    assertion(ExpOut == Out2).

rtpe(LangDef, Inp, ExpABT, ExpOut) :-
    define_language(LangDef, _),
    get_dict(language, LangDef, Language),
    parse_expr(Language, Inp, ABT),
    !,
    assertion(ExpABT = ABT),
    emit_expr(Language, ABT, Out),
    assertion(ExpOut = Out),
    % above may have normalized: ensure it is still parseable
    parse_expr(Language, Out, ABT2),
    !,
    assertion(ABT == ABT2),
    emit_expr(Language, ABT2, Out2),
    assertion(ExpOut == Out2).


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
          expop(or ⦂ a → a → bool, infix(chrs('|')), emit_infix("|")),
          expop(add ⦂ number → number → number, infix(chrs('+')), emit_infix("+")),
          expop(sub ⦂ number → number → number, infix(chrs('-')), emit_infix("-")),
          % nb. 'xor' and 'div' are higher-level builtins and will not parse
          % properly if they are used as the Op.
          expop(exor ⦂ number → number → number, infix(chrs('^')), emit_infix("^")),
          expop(cmpeq ⦂ a → a → bool, infix(chrs('==')), emit_infix("==")),
          expop(const ⦂ a → b → a, [[]>>lexeme(word(const)),
                                    []>>chrs('('),
                                    subexpr,
                                    []>>lexeme(chrs(',')),
                                    subexpr,
                                    []>>lexeme(chrs(')'))
                                   ],
                [F,[A,B],T]>>fmt_str(T, '~w(~w, ~w)', [F, A, B])),
          expop(id ⦂ a → a,
                [[]>>lexeme(word('id')), subexpr],
                [F,[A],T]>>fmt_str(T, 'id ~w', [A]))
          %% expop(call(Fn),
          %%       [[Fn]>>lexeme(word(Fn)),
          %%        chrs('('), subexpr, chrs(','), subexpr, chrs(')')],
          %%      [call(F),[A,B],T]>>fmt_str(T, '~w(~w, ~w)', [F, A, B]))
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
    rtpe(LangDef1, "32 + 19 - 54 + 87 ^ 69966996",
         op(add(term(num(32), number),
                op(sub(term(num(19), number),
                       op(add(term(num(54), number),
                              op(exor(term(num(87), number),
                                      term(num(69966996), number)), number)),
                          number)),
                   number)),
            number),
        "(32 + (19 - (54 + (87 ^ 69966996))))").

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

test(infix_expr_bool_badtypes,
     [nondet,
      error(invalid_term_type(testlang, number, term(lit(false), bool)))
     ]) :-
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

test(fixed_incompat_vars,
     [nondet,
      error(invalid_term_type(testlang, number,
                               op(const(term(ident("other_thing"), bool),
                                        op(cmpeq(term(ident("other_thing"), bool),
                                                 term(lit(true), bool)),
                                           bool)),
                                  bool)))
     ]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "some_num + const(other_thing, other_thing == true)",
         op(other_thing, bool),
         "other_thing is a bool, but add requires a number").

test(fixed_incompat_vars_reverse,
     [nondet,
      error(invalid_term_type(testlang, number,
                               op(const(term(ident("other_thing"),bool),
                                        op(cmpeq(term(ident("other_thing"), bool),
                                                 term(lit(true),bool)),
                                           bool)),
                                  bool)))
     ]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(other_thing, other_thing == true) + some_num",
         op(other_thing, bool),
         "other_thing is a bool, but add requires a number").

test(fixed_incompat_vars_repair, [nondet]) :-
    langdef1(LangDef1),
    get_dict(language, LangDef1, L1),
    atom_concat(L1, not, L2),
    assertz(exprlang:type_repair(L1, Env, term(ident("other_thing"), bool),
                                 % doesn't match expr, should be ignored
                                 _ExprType1,
                                 term(ident("failed_test1"), repair1), Env), A1),
    assertz(exprlang:type_repair(L2, Env,
                                 op(const(term(ident("other_thing"),bool),
                                        op(cmpeq(term(ident("other_thing"), bool),
                                                 term(lit(true),bool)),
                                           bool)),
                                    bool),
                                 % doesn't match language, should be ignored
                                 _ExprType2,
                                 term(ident("failed_test2"), repair2), Env), A2),
    assertz(exprlang:type_repair(L1, Env,
                                 op(const(term(ident("other_thing"),bool),
                                        op(cmpeq(term(ident("other_thing"), bool),
                                                 term(lit(true),bool)),
                                           bool)),
                                    bool),
                                 bool,
                                 % doesn't match expected exprtype, should be ignored
                                 term(ident("failed_test3"), repair3), Env), A3),
    assertz(exprlang:type_repair(L1, Env,
                                 op(const(term(ident("other_thing"),bool),
                                        op(cmpeq(term(ident("other_thing"), bool),
                                                 term(lit(true),bool)),
                                           bool)),
                                    bool),
                                 number,
                                 % this is the one that should fix it, by
                                 % replacing the matched term with a term
                                 % matching the type in the resulting ABT:
                                 term(num(99), number),
                                 Env), A4),
    rtpe(LangDef1, "const(other_thing, other_thing == true) + some_num",
         % Because of the rewrite, the output is very different from the input:
         op(add(term(num(99), number), term(ident("some_num"), number)), number),
         "(99 + some_num)", number),
    erase(A1), erase(A2), erase(A3), erase(A4).

test(result_determines_vartype, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(some_num, true)",
         op(const(term(ident("some_num"), bool),
                  term(lit(true), bool)),
            bool),
         "const(some_num, true)",
        bool).

test(result_determines_vartypes_id_syntax, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(const(id some_num, other), other == 33)",
         op(const(op(const(op(id(term(ident("some_num"), bool)), bool),
                           term(ident("other"), number)),
                     bool),
                  op(cmpeq(term(ident("other"), number),
                           term(num(33), number)),
                     bool)),
            bool),
         "const(const(id some_num, other), (other == 33))",
        bool).

test(infix_inline, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "true | const(some_var, ignored)",
         op(or(term(lit(true), bool),
               op(const(term(ident("some_var"), bool),
                        term(ident("ignored"), type_unassigned('⚲T1'))),
                  bool)),
            bool),
         "(true | const(some_var, ignored))").

test(result_determines_vartypes, [nondet]) :-
    langdef1(LangDef1),
    rtpe(LangDef1, "const(const(some_num, other), other == 33)",
         op(const(op(const(term(ident("some_num"), bool),
                           term(ident("other"), number)),
                     bool),
                  op(cmpeq(term(ident("other"), number),
                           term(num(33), number)),
                     bool)),
            bool),
         "const(const(some_num, other), (other == 33))",
        bool).

%% % KWQ: should enter name into Env as binding name to type.  Then subsequent uses can validate it.  So in the LangDef, it doesn't have a type.  Use a completely separate parser??
%% test(named_function, [nondet]) :-
%%     langdef1(LangDef1),
%%     rtpe(LangDef1, "const(const(some_num, other), other == 33)",
%%          op(const(op(const(term(ident("some_num"), bool),
%%                            term(ident("other"), number)),
%%                      bool),
%%                   op(cmpeq(term(ident("other"), number),
%%                            term(num(33), number)),
%%                      bool)),
%%             bool),
%%          "const(foo(some_num, other), (other == 33))",
%%         bool).
