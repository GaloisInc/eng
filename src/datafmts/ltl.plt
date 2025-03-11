:- begin_tests(ltl).
:- use_module("ltl").
:- use_module(library(strings)).

test(simple_expr, [nondet]) :-
    Inp = "shutdown_requested",
    parse_ltl(Inp, AST),
    assertion(AST == boolid("shutdown_requested")),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "shutdown_requested").

test(paren_expr, [nondet]) :-
    Inp = "(shutdown_requested)",
    parse_ltl(Inp, AST),
    assertion(AST == boolid("shutdown_requested")),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "shutdown_requested").

test(negated_expr, [nondet]) :-
    Inp = "! (shutdown_requested)",
    parse_ltl(Inp, AST),
    assertion(AST == not(boolid("shutdown_requested"))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "not (shutdown_requested)").

test(z_expr, [nondet]) :-
    Inp = "(Z (! startup))",
    parse_ltl(Inp, AST),
    assertion(AST == ltlZ(not(boolid("startup")))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "ZtoPre(not (startup))").

test(bool_or_expr, [nondet]) :-
    Inp = "((shutdown_running) | startup)",
    parse_ltl(Inp, AST),
    assertion(AST == or(boolid("shutdown_running"), boolid("startup"))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "(shutdown_running or startup)").

test(bool3_expr, [nondet]) :-
    Inp = "((shutdown_running) | (startup & (Z (! startup))))",
    parse_ltl(Inp, AST),
    assertion(AST == or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup")))))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "(shutdown_running or (startup and ZtoPre(not (startup))))").

test(bool3_expr_extra_parens, [nondet]) :-
    Inp = "(((((shutdown_running)) | (startup & (((Z (! ((((startup))))))))))))",
    parse_ltl(Inp, AST),
    assertion(AST == or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup")))))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == "(shutdown_running or (startup and ZtoPre(not (startup))))").

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
    assertion(CoCo == "(YtoPre((shutdown_requested and (YtoPre(not (shutdown_requested)) or (startup and ZtoPre(not (startup)))))) => (shutdown_running or (startup and ZtoPre(not (startup)))))").

test(upon_next_expr, [nondet]) :-
    %% Upon not_y and l the a shall at the next timepoint satisfy m & i >= 0.
    %% Inp is ptExpanded_fetched with var replacements
    Inp = "(H ((Y ((not_y & l) & (Z (! (not_y & l))))) -> ((m & (i >= 0)) | (! (Y TRUE)))))",
    CoCoOut = "H((YtoPre(((not_y and l) and ZtoPre(not ((not_y and l))))) => ((m and (i >= 0)) or not (YtoPre(true)))))",
    parse_ltl(Inp, AST),
    assertion(AST == ltlH(implies(ltlY(and(and(boolid("not_y"), boolid("l")),
                                           ltlZ(not(and(boolid("not_y"),
                                                        boolid("l")))))),
                                  or(and(boolid("m"), ge(id("i"), val('0'))),
                                     not(ltlY(true)))))),
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == CoCoOut).

test(in_upon_next_expr, [nondet]) :-
    %% In startup upon shutdown_requested the Starter shall at the next timepoint satisfy shutdown_running.
    %% Inp is ptExpanded_fetched with var replacements
    Inp = "((H (((! startup) & (Y startup)) -> (Y (((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))) -> ((shutdown_running) | (startup & (Z (! startup))))) S (((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))) -> ((shutdown_running) | (startup & (Z (! startup))))) & (startup & (Z (! startup)))))))) & (((! ((! startup) & (Y startup))) S ((! ((! startup) & (Y startup))) & (startup & (Z (! startup))))) -> (((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))) -> ((shutdown_running) | (startup & (Z (! startup))))) S (((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))) -> ((shutdown_running) | (startup & (Z (! startup))))) & (startup & (Z (! startup)))))))",
    CoCoExp = "(H(((not (startup) and YtoPre(startup)) => YtoPre(SI((YtoPre((shutdown_requested and (YtoPre(not (shutdown_requested)) or (startup and ZtoPre(not (startup)))))) => (shutdown_running or (startup and ZtoPre(not (startup))))), ((YtoPre((shutdown_requested and (YtoPre(not (shutdown_requested)) or (startup and ZtoPre(not (startup)))))) => (shutdown_running or (startup and ZtoPre(not (startup))))) and (startup and ZtoPre(not (startup)))))))) and (SI(not ((not (startup) and YtoPre(startup))), (not ((not (startup) and YtoPre(startup))) and (startup and ZtoPre(not (startup))))) => SI((YtoPre((shutdown_requested and (YtoPre(not (shutdown_requested)) or (startup and ZtoPre(not (startup)))))) => (shutdown_running or (startup and ZtoPre(not (startup))))), ((YtoPre((shutdown_requested and (YtoPre(not (shutdown_requested)) or (startup and ZtoPre(not (startup)))))) => (shutdown_running or (startup and ZtoPre(not (startup))))) and (startup and ZtoPre(not (startup)))))))",
    parse_ltl(Inp, AST),
    % Not validing AST itself: it's rather large, and there's not significant
    % value to doing so.
    emit_CoCoSpec(AST, CoCo),
    assertion(CoCo == CoCoExp).
