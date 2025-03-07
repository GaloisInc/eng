:- begin_tests(lando_fret).
:- use_module("lando_fret").
:- use_module(library(strings)).

test(simple_expr, [nondet]) :-
    Inp = "shutdown_requested",
    transform_to_AST(Inp, AST),
    assertion(AST == id("shutdown_requested")).

test(paren_expr, [nondet]) :-
    Inp = "(shutdown_requested)",
    transform_to_AST(Inp, AST),
    assertion(AST == id("shutdown_requested")).

test(negated_expr, [nondet]) :-
    Inp = "! (shutdown_requested)",
    transform_to_AST(Inp, AST),
    assertion(AST == not(boolid("shutdown_requested"))).

test(z_expr, [nondet]) :-
    Inp = "(Z (! startup))",
    transform_to_AST(Inp, AST),
    assertion(AST == ltlZ(not(boolid("startup"))).

test(bool_or_expr, [nondet]) :-
    Inp = "((shutdown_running) | startup)",
    transform_to_AST(Inp, AST),
    assertion(AST == not(boolid("shutdown_requested"))).

test(bool3_expr, [nondet, blocked(true)]) :-
    Inp = "((shutdown_running) | (startup & (Z (! startup))))",
    transform_to_AST(Inp, AST),
    assertion(AST == not(boolid("shutdown_requested"))).
