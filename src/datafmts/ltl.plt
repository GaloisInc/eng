:- begin_tests(ltl).
:- use_module("ltl").
:- use_module(library(strings)).

test(simple_expr, [nondet]) :-
    Inp = "shutdown_requested",
    parse_ltl(Inp, AST),
    assertion(AST == boolid("shutdown_requested")),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "__shutdown_requested").

test(paren_expr, [nondet]) :-
    Inp = "(shutdown_requested)",
    parse_ltl(Inp, AST),
    assertion(AST == boolid("shutdown_requested")),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "__shutdown_requested").

test(negated_expr, [nondet]) :-
    Inp = "! (shutdown_requested)",
    parse_ltl(Inp, AST),
    assertion(AST == not(boolid("shutdown_requested"))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "not (__shutdown_requested)").

test(z_expr, [nondet]) :-
    Inp = "(Z (! startup))",
    parse_ltl(Inp, AST),
    assertion(AST == ltlZ(not(boolid("startup")))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "ZtoPre(not (__startup))").

test(bool_or_expr, [nondet]) :-
    Inp = "((shutdown_running) | startup)",
    parse_ltl(Inp, AST),
    assertion(AST == or(boolid("shutdown_running"), boolid("startup"))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "(__shutdown_running or __startup)").

test(bool3_expr, [nondet]) :-
    Inp = "((shutdown_running) | (startup & (Z (! startup))))",
    parse_ltl(Inp, AST),
    assertion(AST == or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup")))))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "(__shutdown_running or (__startup and ZtoPre(not (__startup))))").

test(bool3_expr_extra_parens, [nondet]) :-
    Inp = "(((((shutdown_running)) | (startup & (((Z (! ((((startup))))))))))))",
    parse_ltl(Inp, AST),
    assertion(AST == or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup")))))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "(__shutdown_running or (__startup and ZtoPre(not (__startup))))").

test(bool_expr_mid_term, [nondet]) :-
    Inp = "((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))
        ) -> ((shutdown_running) | (startup & (Z (! startup)))))",
    parse_ltl(Inp, AST),
    assertion(AST == implies(ltlY(and(boolid("shutdown_requested"),
                                      or(ltlY(not(boolid("shutdown_requested"))),
                                         and(boolid("startup"),
                                             ltlZ(not(boolid("startup"))))))),
              or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup"))))))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "(YtoPre((__shutdown_requested and (YtoPre(not (__shutdown_requested)) or (__startup and ZtoPre(not (__startup)))))) => (__shutdown_running or (__startup and ZtoPre(not (__startup)))))").

test(bool_expr_mid_term, [nondet]) :-
    %% Upon not_y and l the a shall at the next timepoint satisfy m & i >= 0.
    %% Inp is ptExpanded_fetched with var replacements
    Inp = "(H ((Y ((not_y & l) & (Z (! (not_y & l))))) -> ((m & (i >= 0)) | (! (Y TRUE)))))",
    CoCoOut = "H((YtoPre(((__not_y and __l) and ZtoPre(not ((__not_y and __l))))) => ((__m and (__i >= 0)) or not (YtoPre(true)))))",
    parse_ltl(Inp, AST),
    assertion(AST == ltlH(implies(ltlY(and(and(boolid("not_y"), boolid("l")),
                                           ltlZ(not(and(boolid("not_y"),
                                                        boolid("l")))))),
                                  or(and(boolid("m"), ge(id("i"), val('0'))),
                                     not(ltlY(true)))))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == CoCoOut).
