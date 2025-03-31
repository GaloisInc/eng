:- begin_tests(ltl).
:- use_module("ltl").
:- use_module(library(strings)).

test(simple_expr, [nondet]) :-
    Inp = "shutdown_requested",
    parse_ltl(Inp, AST),
    assertion(AST == boolid("shutdown_requested")),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "shutdown_requested").

test(paren_expr, [nondet]) :-
    Inp = "(shutdown_requested)",
    parse_ltl(Inp, AST),
    !,
    assertion(AST == boolid("shutdown_requested")),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "shutdown_requested").

test(negated_expr, [nondet]) :-
    Inp = "! (shutdown_requested)",
    parse_ltl(Inp, AST),
    !,
    assertion(AST == not(boolid("shutdown_requested"))),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "not (shutdown_requested)").

test(z_expr, [nondet]) :-
    Inp = "(Z (! startup))",
    parse_ltl(Inp, AST),
    !,
    assertion(AST == ltlZ(not(boolid("startup")))),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "ZtoPre(not (startup))").

test(bool_or_expr, [nondet]) :-
    Inp = "((shutdown_running) | startup)",
    parse_ltl(Inp, AST),
    !,
    assertion(AST == or(boolid("shutdown_running"), boolid("startup"))),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "(shutdown_running or startup)").

test(bool_triple_term, [nondet]) :-
    Inp = "stopping & !shutdown_running & !startup",
    parse_ltl(Inp, AST),
    !,
    assertion(AST == and(and(boolid("stopping"),
                             not(boolid("shutdown_running"))),
                         not(boolid("startup"))
                        )),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "((stopping and not (shutdown_running)) and not (startup))").

test(bool3_expr, [nondet]) :-
    Inp = "((shutdown_running) | (startup & (Z (! startup))))",
    parse_ltl(Inp, AST),
    !,
    assertion(AST == or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup")))))),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "(shutdown_running or (startup and ZtoPre(not (startup))))").

test(bool3_expr_extra_parens, [nondet]) :-
    Inp = "(((((shutdown_running)) | (startup & (((Z (! ((((startup))))))))))))",
    parse_ltl(Inp, AST),
    !,
    assertion(AST == or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup")))))),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "(shutdown_running or (startup and ZtoPre(not (startup))))").

test(bool_expr_4, [nondet]) :-
    Inp = "((alarm_enabled & disable_alarm) & (Z (! (alarm_enabled & disable_alarm))))",
    parse_ltl(Inp, AST),
    !,
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "((alarm_enabled and disable_alarm) and ZtoPre(not ((alarm_enabled and disable_alarm))))").

test(bool_expr_5, [nondet]) :-
    Inp = "(O[<=2 ] ((alarm_enabled & disable_alarm) & (Z (! (alarm_enabled & disable_alarm)))))",
    parse_ltl(Inp, AST),
    !,
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "OT(2, 0, ((alarm_enabled and disable_alarm) and ZtoPre(not ((alarm_enabled and disable_alarm)))))").

test(bool_expr_6, [nondet]) :-
    Inp = "(O[=2 +1] alarm_enabled)",
    parse_ltl(Inp, AST),
    !,
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "OT(3, 3, alarm_enabled)").

test(bool_expr_with_ltl_func, [nondet]) :-
    Inp = "(H ((Y (awake & ((H[0,2] wet) & (H[0,1] (Y TRUE))))) -> ((noise = croaking) | (! (Y TRUE)))))",
    % n.b. (! (Y TRUE)) is optimized to (Z FALSE)
    CoCoSpec = "H((YtoPre((awake and (HT(2, 0, wet) and HT(1, 0, YtoPre(true))))) => ((noise = croaking) or ZtoPre(false))))",
    parse_ltl(Inp, AST0),
    !,
    emit_ltl(AST0, Inp2),
    % n.b. cannot compare Inp and Inp2: parentheses and other optimizations
    parse_ltl(Inp2, AST),
    !,
    assertion(AST0 == AST),
    writeln(ltl_parsed),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, CoCoSpec).

test(bool3_expr_unary_timed_bound, [nondet]) :-
    Inp = "((H ((O[<=2 ] ((alarm_enabled & disable_alarm) & (Z (! (alarm_enabled & disable_alarm))))) -> ((H (! (alarm_enabled & disable_alarm))) | (! (alarm_disabled))))) & (H ((O[=2 +1] (((alarm_enabled & disable_alarm) & (Z (! (alarm_enabled & disable_alarm)))) & (! (alarm_disabled)))) -> (O[<2 +1] ((Z FALSE) | (alarm_disabled))))))",
    parse_ltl(Inp, AST0),
    !,
    emit_ltl(AST0, Inp2),
    % n.b. cannot compare Inp and Inp2: parentheses and other optimizations
    parse_ltl(Inp2, AST),
    !,
    assertion(AST0 == AST),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "(H((OT(2, 0, ((alarm_enabled and disable_alarm) and ZtoPre(not ((alarm_enabled and disable_alarm))))) => (H(not ((alarm_enabled and disable_alarm))) or not (alarm_disabled)))) and H((OT(3, 3, (((alarm_enabled and disable_alarm) and ZtoPre(not ((alarm_enabled and disable_alarm)))) and not (alarm_disabled))) => OT((3 - 1), 0, (ZtoPre(false) or alarm_disabled)))))").


test(bool_expr_mid_term, [nondet]) :-
    Inp = "((Y ((shutdown_requested) & ((Y (! (shutdown_requested))) | (startup & (Z (! startup)))))
        ) -> ((shutdown_running) | (startup & (Z (! startup)))))",
    parse_ltl(Inp, AST),
    !,
    assertion(AST == implies(ltlY(and(boolid("shutdown_requested"),
                                      or(ltlY(not(boolid("shutdown_requested"))),
                                         and(boolid("startup"),
                                             ltlZ(not(boolid("startup"))))))),
              or(boolid("shutdown_running"),
                        and(boolid("startup"),
                           ltlZ(not(boolid("startup"))))))),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, "(YtoPre((shutdown_requested and (YtoPre(not (shutdown_requested)) or (startup and ZtoPre(not (startup)))))) => (shutdown_running or (startup and ZtoPre(not (startup)))))").

test(upon_next_expr, [nondet]) :-
    %% Upon not_y and l the a shall at the next timepoint satisfy m & i >= 0.
    %% Inp is ptExpanded_fetched with var replacements
    Inp = "(H ((Y ((not_y & l) & (Z (! (not_y & l))))) -> ((m & (i >= 0)) | (! (Y TRUE)))))",
    % n.b. (! (Y TRUE)) is optimized to (Z FALSE)
    CoCoExp = "H((YtoPre(((not_y and l) and ZtoPre(not ((not_y and l))))) => ((m and (i >= 0)) or ZtoPre(false))))",
    parse_ltl(Inp, AST),
    !,
    assertion(AST == ltlH(implies(ltlY(and(and(boolid("not_y"), boolid("l")),
                                           ltlZ(not(and(boolid("not_y"),
                                                        boolid("l")))))),
                                  or(and(boolid("m"), ge(id("i"), val(0))),
                                     ltlZ(false))))),
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, CoCoExp).

test(in_upon_next_expr, [nondet]) :-
    %% In braking upon start_button the Car shall at the next timepoint satisfy is_moving.
    %% Inp is ptExpanded_fetched with var replacements
    Inp = "((H (((! braking) & (Y braking)) -> (Y (((Y (start_button & ((Y (! start_button)) | (braking & (Z (! braking)))))) -> (is_moving | (braking & (Z (! braking))))) S (((Y (start_button & ((Y (! start_button)) | (braking & (Z (! braking)))))) -> (is_moving | (braking & (Z (! braking))))) & (braking & (Z (! braking)))))))) & (((! ((! braking) & (Y braking))) S ((! ((! braking) & (Y braking))) & (braking & (Z (! braking))))) -> (((Y (start_button & ((Y (! start_button)) | (braking & (Z (! braking)))))) -> (is_moving | (braking & (Z (! braking))))) S (((Y (start_button & ((Y (! start_button)) | (braking & (Z (! braking)))))) -> (is_moving | (braking & (Z (! braking))))) & (braking & (Z (! braking)))))))",
    CoCoExp = "(H(((not (braking) and YtoPre(braking)) => YtoPre(SI((braking and ZtoPre(not (braking))),(YtoPre((start_button and (YtoPre(not (start_button)) or (braking and ZtoPre(not (braking)))))) => (is_moving or (braking and ZtoPre(not (braking))))))))) and (SI((braking and ZtoPre(not (braking))),not ((not (braking) and YtoPre(braking)))) => SI((braking and ZtoPre(not (braking))),(YtoPre((start_button and (YtoPre(not (start_button)) or (braking and ZtoPre(not (braking)))))) => (is_moving or (braking and ZtoPre(not (braking))))))))",
    parse_ltl(Inp, AST0),
    !,
    emit_ltl(AST0, Inp2),
    % n.b. cannot compare Inp and Inp2: parentheses and other optimizations
    parse_ltl(Inp2, AST),
    !,
    assertion(AST0 == AST),
    % Not validing AST itself: it's rather large, and there's not significant
    % value to doing so.
    emit_CoCoSpec(AST, CoCo),
    same_string(CoCo, CoCoExp).

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
