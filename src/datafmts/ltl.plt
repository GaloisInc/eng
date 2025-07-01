:- begin_tests(ltl).
:- use_module(ltl).
:- use_module(lustre).
:- use_module(library(strings)).

check_parsing(Inp, ExpAST, ExpCoCoSpec) :-
    define_ltl_language,
    define_lustre_language,
    writeln(parsing_ltl),
    parse_ltl(Inp, AST),
    !,
    writeln(AST),
    assertion(AST == ExpAST),
    writeln(emitting_ltl),
    emit_ltl(AST, OutLTL),
    writeln(OutLTL),
    writeln(parsing_ltl_again),
    parse_ltl(OutLTL, AST2),
    !,
    assertion(AST2 == ExpAST),
    writeln(emit_cocospec),
    emit_CoCoSpec(AST, OutCoCoSpec),
    same_string(OutCoCoSpec, ExpCoCoSpec).

check_parsing(Inp, ExpCoCoSpec) :-
    define_ltl_language,
    define_lustre_language,
    writeln(parsing_ltl),
    writeln(Inp),
    parse_ltl(Inp, AST),
    !,
    writeln(AST),
    writeln(emitting_ltl),
    emit_ltl(AST, OutLTL),
    writeln(parsing_ltl_again),
    writeln(OutLTL),
    parse_ltl(OutLTL, AST2),
    !,
    assertion(AST2 == AST),
    writeln(emit_cocospec),
    emit_CoCoSpec(AST, OutCoCoSpec),
    same_string(OutCoCoSpec, ExpCoCoSpec).


test(simple_expr, [nondet]) :-
    check_parsing("shutdown_requested",
                  term(ident("shutdown_requested"), type_unassigned('⚲T0')),
                  "shutdown_requested").

test(subst_ident, [nondet]) :-
    check_parsing("$shutdown_requested$",
                  term(ident('$shutdown_requested$'), type_unassigned('⚲T0')),
                  "$shutdown_requested$").


test(paren_expr, [nondet]) :-
    check_parsing("(shutdown_requested)",
                  term(ident("shutdown_requested"), type_unassigned('⚲T0')),
                  "shutdown_requested").

test(negated_expr, [nondet]) :-
    check_parsing("! (shutdown_requested)",
                  op(not(term(ident("shutdown_requested"), bool)), bool),
                  "(not shutdown_requested)").

test(z_expr, [nondet]) :-
    check_parsing("(Z (! startup))",
                  op(ltlZ(op(not(term(ident("startup"), bool)), bool)), bool),
                  "ZtoPre((not startup))").

test(h_expr, [nondet]) :-
    check_parsing("(H (! startup))",
                  op(ltlH(op(not(term(ident("startup"), bool)), bool)), bool),
                  "H((not startup))").

test(bool_or_expr, [nondet]) :-
    check_parsing("((shutdown_running) | startup)",
                  op(or(term(ident("shutdown_running"), bool),
                        term(ident("startup"), bool)), bool),
                  "(shutdown_running or startup)").

test(bool_triple_term, [nondet]) :-
    check_parsing("stopping & !shutdown_running & !startup",
                  op(and(term(ident("stopping"), bool),
                         op(not(op(and(term(ident("shutdown_running"), bool),
                                       op(not(term(ident("startup"), bool)), bool)),
                                   bool)),
                            bool)),
                     bool),
                  % surprise!
                  "(stopping and (not (shutdown_running and (not startup))))").

test(bool_triple_term_parens, [nondet]) :-
    check_parsing("stopping & (!shutdown_running) & (!startup)",
                  op(and(term(ident("stopping"), bool),
                         op(and(op(not(term(ident("shutdown_running"), bool)),
                                   bool),
                                op(not(term(ident("startup"), bool)), bool)),
                            bool)),
                     bool),
                  % this is probably what you wanted
                  "(stopping and ((not shutdown_running) and (not startup)))").

test(bool3_expr, [nondet]) :-
    check_parsing("((shutdown_running) | (startup & (Z (! startup))))",
                  op(or(term(ident("shutdown_running"), bool),
                        op(and(term(ident("startup"), bool),
                               op(ltlZ(op(not(term(ident("startup"), bool)),
                                          bool)),
                                  bool)),
                           bool)),
                     bool),
                  "(shutdown_running or (startup and ZtoPre((not startup))))").

test(bool3_expr_extra_parens, [nondet]) :-
    check_parsing("(((((shutdown_running)) | (startup & (((Z (! ((((startup))))))))))))",
                  op(or(term(ident("shutdown_running"), bool),
                        op(and(term(ident("startup"), bool),
                               op(ltlZ(op(not(term(ident("startup"), bool)),
                                          bool)),
                                  bool)),
                           bool)),
                     bool),
                  "(shutdown_running or (startup and ZtoPre((not startup))))").

test(bool_expr_4, [nondet]) :-
    check_parsing("((alarm_enabled & disable_alarm) & (Z (! (alarm_enabled & disable_alarm))))",
                  "((alarm_enabled and disable_alarm) and ZtoPre((not (alarm_enabled and disable_alarm))))").

test(bool_expr_5, [nondet]) :-
    check_parsing("(O[<=23456 ] ((alarm_enabled & disable_alarm) & (Z (! (alarm_enabled & disable_alarm)))))",
                  "OT(23456, 0, ((alarm_enabled and disable_alarm) and ZtoPre((not (alarm_enabled and disable_alarm)))))").

test(bool_expr_6, [nondet]) :-
    check_parsing("(O[=2 +1] alarm_enabled)",
                  "OT(3, 3, alarm_enabled)").

test(bool_expr_7, [nondet]) :-
    check_parsing("shutdown_running S startup",
                  op(ltlS(term(ident("shutdown_running"), bool),
                          term(ident("startup"), bool)),
                     bool),
                  % note that CoCoSpec reverses the order of arguments to S and
                  % SI for helpers
                  "S(startup, shutdown_running)").

test(bool_expr_8, [nondet]) :-
    check_parsing("((shutdown_running) S (startup & (Z (! startup))))",
                  op(ltlS(term(ident("shutdown_running"), bool),
                          op(and(term(ident("startup"), bool),
                                 op(ltlZ(op(not(term(ident("startup"), bool)),
                                            bool)),
                                    bool)),
                             bool)),
                     bool),
                  % note that CoCoSpec reverses the order of arguments to S and
                  % SI for helpers
                  "S((startup and ZtoPre((not startup))), shutdown_running)").

test(bool_opt_9, [nondet]) :-
    check_parsing("(((alarm_enabled & (! FALSE) | (! TRUE) & TRUE | FALSE) & (! (! disable_alarm))) & (! (! (Z (! (! FALSE))))))",
                  % !(! X) optimizes to X
                  % (! TRUE) optimizes to FALSE
                  % (! FALSE) optimizes to TRUE
                  % !(Z FALSE) optimizes to (Y TRUE)
                  % !(Y TRUE) optimizes to (Z FALSE)
                  op(and(op(and(term(ident("alarm_enabled"), bool),
                                term(ident("disable_alarm"), bool)), bool),
                         op(ltlZ(term(lit(false), bool)), bool)),
                     bool),
                  "((alarm_enabled and disable_alarm) and ZtoPre(false))").

test(bool_expr_with_ltl_func, [nondet]) :-
    check_parsing("(H ((Y (awake & ((H[0,2] wet) & (H[0,1] (Y TRUE))))) -> ((noise = croaking) | (! (Y TRUE)))))",
    % n.b. (! (Y TRUE)) is optimized to (Z FALSE)
                  "H((YtoPre((awake and (HT(2, 0, wet) and HT(1, 0, YtoPre(true))))) => ((noise = croaking) or ZtoPre(false))))").

test(bool3_expr_unary_timed_bound, [nondet]) :-
    check_parsing("((H ((O[<=2 ] ((alarm_enabled & disable_alarm) & (Z (! (alarm_enabled & disable_alarm))))) -> ((H (! (alarm_enabled & disable_alarm))) | (! (alarm_disabled))))) & (H ((O[=2 +1] (((alarm_enabled & disable_alarm) & (Z (! (alarm_enabled & disable_alarm)))) & (! (alarm_disabled)))) -> (O[<2 +1] ((Z FALSE) | (alarm_disabled))))))",
                  "(H((OT(2, 0, ((alarm_enabled and disable_alarm) and ZtoPre((not (alarm_enabled and disable_alarm))))) => (H((not (alarm_enabled and disable_alarm))) or (not alarm_disabled)))) and H((OT(3, 3, (((alarm_enabled and disable_alarm) and ZtoPre((not (alarm_enabled and disable_alarm)))) and (not alarm_disabled))) => OT((3 - 1), 0, (ZtoPre(false) or alarm_disabled)))))").


test(bool_expr_mid_term, [nondet]) :-
    check_parsing("((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))
        ) -> ((shutdown_running) | (startup & (Z (! startup)))))",
                  op(implies(op(ltlY(op(and(term(ident("shutdown_requested"),
                                                 bool),
                                            op(or(op(ltlY(op(not(term(ident("shutdown_requested"),
                                                                      bool)),
                                                             bool)),
                                                     bool),
                                                  op(and(term(ident("startup"), bool),
                                                         op(ltlZ(op(not(term(ident("startup"), bool)),
                                                                    bool)),
                                                            bool)),
                                                     bool)),
                                               bool)),
                                        bool)),
                                bool),
                             op(or(term(ident("shutdown_running"), bool),
                                   op(and(term(ident("startup"), bool),
                                          op(ltlZ(op(not(term(ident("startup"), bool)),
                                                     bool)),
                                             bool)),
                                      bool)),
                                bool)),
                     bool),
    "(YtoPre((shutdown_requested and (YtoPre((not shutdown_requested)) or (startup and ZtoPre((not startup)))))) => (shutdown_running or (startup and ZtoPre((not startup)))))").

test(upon_next_expr, [nondet]) :-
    %% Upon not_y and l the a shall at the next timepoint satisfy m & i >= 0.
    %% Inp is ptExpanded_fetched with var replacements
    check_parsing("(H ((Y ((not_y & l) & (Z (! (not_y & l))))) -> ((m & (i >= 0)) | (! (Y TRUE)))))",
                  % n.b. (! (Y TRUE)) is supposed to be optimized to (Z FALSE)
                  op(ltlH(op(implies(op(ltlY(op(and(op(and(term(ident("not_y"), bool),
                                                           term(ident("l"), bool)),
                                                       bool),
                                                    op(ltlZ(op(not(op(and(term(ident("not_y"), bool),
                                                                          term(ident("l"), bool)),
                                                                      bool)),
                                                               bool)),
                                                       bool)),
                                                bool)),
                                        bool),
                                     op(or(op(and(term(ident("m"), bool),
                                                  op(gteq(term(ident("i"), integer),
                                                          term(num(0), integer)),
                                                     bool)),
                                              bool),
                                           op(ltlZ(term(lit(false), bool)),
                                              bool)),
                                        bool)),
                             bool)),
                     bool),
                  "H((YtoPre(((not_y and l) and ZtoPre((not (not_y and l))))) => ((m and (i >= 0)) or ZtoPre(false))))").


test(in_upon_next_expr, [nondet]) :-
    %% In braking upon start_button the Car shall at the next timepoint satisfy is_moving.
    %% see frettish.plt in_upon_next_satisfy test
    %% in,regular,next,satisfaction
    %% Inp is ptExpanded_fetched with var replacements
    check_parsing("((H (((! braking) & (Y braking)) -> (Y (((Y (start_button & ((Y (! start_button)) | (braking & (Z (! braking)))))) -> (is_moving | (braking & (Z (! braking))))) S (((Y (start_button & ((Y (! start_button)) | (braking & (Z (! braking)))))) -> (is_moving | (braking & (Z (! braking))))) & (braking & (Z (! braking)))))))) & (((! ((! braking) & (Y braking))) S ((! ((! braking) & (Y braking))) & (braking & (Z (! braking))))) -> (((Y (start_button & ((Y (! start_button)) | (braking & (Z (! braking)))))) -> (is_moving | (braking & (Z (! braking))))) S (((Y (start_button & ((Y (! start_button)) | (braking & (Z (! braking)))))) -> (is_moving | (braking & (Z (! braking))))) & (braking & (Z (! braking)))))))",

                  % Without optimization, the above should directly convert to:
                  %% "(H((((not braking) and YtoPre(braking)) => YtoPre(S((YtoPre((start_button and (YtoPre((not start_button)) or (braking and ZtoPre((not braking)))))) => (is_moving or (braking and ZtoPre((not braking))))), ((YtoPre((start_button and (YtoPre((not start_button)) or (braking and ZtoPre((not braking)))))) => (is_moving or (braking and ZtoPre((not braking))))) and (braking and ZtoPre((not braking)))))))) and (S((not ((not braking) and YtoPre(braking))), ((not ((not braking) and YtoPre(braking))) and (braking and ZtoPre((not braking))))) => S((YtoPre((start_button and (YtoPre((not start_button)) or (braking and ZtoPre((not braking)))))) => (is_moving or (braking and ZtoPre((not braking))))), ((YtoPre((start_button and (YtoPre((not start_button)) or (braking and ZtoPre((not braking)))))) => (is_moving or (braking and ZtoPre((not braking))))) and (braking and ZtoPre((not braking)))))))").

                  %% However, with term rewriting optimization, it reduces to the
                  %% following (note that CoCoSpec reverses the order of
                  %% arguments to S and SI for helpers).  Note that original FRET
                  %% does not place a space between the two arguments of SI
                  %% whereas eng does, so there are three added spaces below.
                  "(H((((not braking) and YtoPre(braking)) => YtoPre(SI((braking and ZtoPre((not braking))), (YtoPre((start_button and (YtoPre((not start_button)) or (braking and ZtoPre((not braking)))))) => (is_moving or (braking and ZtoPre((not braking))))))))) and (SI((braking and ZtoPre((not braking))), (not ((not braking) and YtoPre(braking)))) => SI((braking and ZtoPre((not braking))), (YtoPre((start_button and (YtoPre((not start_button)) or (braking and ZtoPre((not braking)))))) => (is_moving or (braking and ZtoPre((not braking))))))))").

same_string(S, S).
same_string(A, B) :-
    string_concat(A, BE, B),
    string_length(BE, L),
    format('first string is ~w characters too short from: ~w~n', [L, B]),
    !,
    assertion(A == B).
same_string(A, B) :-
    string_concat(B, AE, A),
    string_length(AE, L),
    format('second string is ~w characters too short from: ~w~n', [L, A]),
    !,
    assertion(A == B).
same_string(A, B) :-
    string_concat(Same, Dif1, A),
    string_concat(Same, Dif2, B),
    string_chars(Dif1, [D1|_]),
    string_chars(Dif2, [D2|_]),
    \+ D1 == D2,
    string_length(Same, L),
    format('first and second string are the same for ~w chars~n  same: ~w~n  first  tail: ~w~n  second tail: ~w~n',
           [ L, Same, Dif1, Dif2 ]),
    assertion(A == B).
