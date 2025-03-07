:- begin_tests(lando_fret).
:- use_module("lando_fret").
:- use_module(library(strings)).

test(simple_expr, [nondet]) :-
    Inp = "shutdown_requested",
    transform_to_AST(Inp, AST),
    assertion(AST == boolid("shutdown_requested")).

test(paren_expr, [nondet]) :-
    Inp = "(shutdown_requested)",
    transform_to_AST(Inp, AST),
    assertion(AST == boolid("shutdown_requested")).

test(negated_expr, [nondet]) :-
    Inp = "! (shutdown_requested)",
    transform_to_AST(Inp, AST),
    assertion(AST == not(boolid("shutdown_requested"))).

test(z_expr, [nondet]) :-
    Inp = "(Z (! startup))",
    transform_to_AST(Inp, AST),
    assertion(AST == ltlZ(not(boolid("startup")))).

test(bool_or_expr, [nondet]) :-
    Inp = "((shutdown_running) | startup)",
    transform_to_AST(Inp, AST),
    assertion(AST == or(boolid("shutdown_running"), boolid("startup"))).

test(bool3_expr, [nondet]) :-
    Inp = "((shutdown_running) | (startup & (Z (! startup))))",
    transform_to_AST(Inp, AST),
    assertion(AST == or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup")))))).

test(bool3_expr_extra_parens, [nondet]) :-
    Inp = "(((((shutdown_running)) | (startup & (((Z (! ((((startup))))))))))))",
    transform_to_AST(Inp, AST),
    assertion(AST == or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup")))))).

test(bool_expr_mid_term, [nondet]) :-
    Inp = "((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))
        ) -> ((shutdown_running) | (startup & (Z (! startup)))))",
    transform_to_AST(Inp, AST),
    assertion(AST == implies(ltlY(and(boolid("shutdown_requested"),
                                      or(ltlY(not(boolid("shutdown_requested"))),
                                         and(boolid("startup"),
                                             ltlZ(not(boolid("startup"))))))),
              or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup"))))))).

test(bool_full_expr, [nondet]) :-
    Inp = "((H (((! startup) & (Y startup)) -> (Y (((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))) -> ((shutdown_running) | (startup & (Z (! startup))))) S (((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))) -> ((shutdown_running) | (startup & (Z (! startup))))) & (startup & (Z (! startup)))))))) & (((! ((! startup) & (Y startup))) S ((! ((! startup) & (Y startup))) & (startup & (Z (! startup))))) -> (((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))) -> ((shutdown_running) | (startup & (Z (! startup))))) S (((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))) -> ((shutdown_running) | (startup & (Z (! startup))))) & (startup & (Z (! startup)))))))",
    transform_to_AST(Inp, AST),
    assertion(AST ==
              and(ltlH(implies(and(not(boolid("startup")),ltlY(boolid("startup"))),ltlY(binS(implies(ltlY(and(boolid("shutdown_requested"),or(ltlY(not(boolid("shutdown_requested"))),and(boolid("startup"),ltlZ(not(boolid("startup"))))))),or(boolid("shutdown_running"),and(boolid("startup"),ltlZ(not(boolid("startup")))))),and(implies(ltlY(and(boolid("shutdown_requested"),or(ltlY(not(boolid("shutdown_requested"))),and(boolid("startup"),ltlZ(not(boolid("startup"))))))),or(boolid("shutdown_running"),and(boolid("startup"),ltlZ(not(boolid("startup")))))),and(boolid("startup"),ltlZ(not(boolid("startup"))))))))),
                  implies(
                      binS(not(and(not(boolid("startup")),ltlY(boolid("startup")))),and(not(and(not(boolid("startup")),ltlY(boolid("startup")))),and(boolid("startup"),ltlZ(not(boolid("startup")))))),
                      binS(implies(ltlY(and(boolid("shutdown_requested"),or(ltlY(not(boolid("shutdown_requested"))),and(boolid("startup"),ltlZ(not(boolid("startup"))))))),or(boolid("shutdown_running"),and(boolid("startup"),ltlZ(not(boolid("startup")))))),and(implies(ltlY(and(boolid("shutdown_requested"),or(ltlY(not(boolid("shutdown_requested"))),and(boolid("startup"),ltlZ(not(boolid("startup"))))))),or(boolid("shutdown_running"),and(boolid("startup"),ltlZ(not(boolid("startup")))))),and(boolid("startup"),ltlZ(not(boolid("startup"))))))
                  ))).
