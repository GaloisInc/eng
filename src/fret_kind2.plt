:- begin_tests(fret_kind2).
:- use_module(fret_kind2).
:- use_module(library(strings)).

test(empty_no_components, [nondet]) :-
    connected_components([], [], 0, C),
    assertion(C == []).

test(one_rel_one_component, [nondet]) :-
    R1 = _{lando_req:lando_requirement{
                         fret_req:
                         fretment(unused,
                                  condition_info(_{condition: "null"}),
                                  component_info(_{component: "comp1"}),
                                  unused, unused)}},
    connected_components([R1], [], 0, C),
    assertion(C == [comp(0, "comp1", [R1], [])]).

test(one_rel_one_outvar_one_component, [nondet]) :-
    PC = fretish(op(eq(term(ident("var1"), integer),
                       term(val(1), integer)), bool)),
    R1 = _{lando_req:lando_requirement{
                         fret_req:
                         fretment(unused,
                                  condition_info(_{condition: "null"}),
                                  component_info(_{component: "comp1"}),
                                  unused,
                                  response_info(_{post_condition:PC}))}},
    V1 = _{varname: "var1", usage: "Output"},
    connected_components([R1], [V1], 0, C),
    assertion(C == [comp(0, "comp1", [R1], ["var1"])]).

test(one_rel_four_outvar_one_component, [nondet]) :-
    PC = fretish(op(and(op(eq(term(ident("var1"), integer),
                              term(val(1), integer)), bool),
                        op(eq(term(ident("var4"), bool),
                              term(lit(true), bool)), bool)), bool)),
    CC = fretish(op(eq(term(ident("var2"), sphinx__T),
                       term(ident("var3"), sphinx__T)), bool)),
    R1 = _{lando_req:lando_requirement{
                         fret_req:
                         fretment(unused,
                                  condition_info(_{condition: "regular",
                                                   regular_condition: CC}),
                                  component_info(_{component: "comp1"}),
                                  unused,
                                  response_info(_{post_condition:PC}))}},
    V1 = _{varname: "var1", usage: "Output"},
    V2 = _{varname: "var2", usage: "Internal"},
    V3 = _{varname: "var3", usage: "Input"},
    V4 = _{varname: "var4", usage: "Output"},
    connected_components([R1], [V1, V2, V3, V4], 0, C),
    assertion(C == [comp(0, "comp1", [R1], ["var1", "var4"])]).

test(three_rel_four_outvar_two_component, [nondet]) :-
    CC1 = fretish(op(and(op(eq(term(ident("var3"), integer),
                               term(val(34), integer)), bool),
                         op(eq(term(ident("var2"), rocket_T),
                               term(val(87), rocket_T)), bool)), bool)),
    PC1 = fretish(op(or(op(eq(term(ident("var1"), bool),
                              term(lit(false), bool)), bool),
                        op(eq(term(ident("var4"), integer),
                              term(val(1), integer)), bool)),
                     bool)),
    R1 = _{lando_req:lando_requirement{
                         fret_req:
                         fretment(unused,
                                  condition_info(_{condition: "regular",
                                                   regular_condition: CC1}),
                                  component_info(_{component: "comp1"}),
                                  unused,
                                  response_info(_{post_condition:PC1}))}},
    CC2 = fretish(op(eq(term(ident("var3"), integer),
                        term(val(33), integer)), bool)),
    PC2 = fretish(op(eq(term(ident("var4"), integer),
                        term(ident("var3"), integer)), bool)),
    R2 = _{lando_req:lando_requirement{
                         fret_req:
                         fretment(unused,
                                  condition_info(_{condition: "regular",
                                                   regular_condition: CC2}),
                                  component_info(_{component: "comp1"}),
                                  unused,
                                  response_info(_{post_condition:PC2}))}},
    CC3 = CC2,
    PC3 = fretish(op(eq(term(ident("var2"), rocket_T),
                        term(val(87), rocket_T)), bool)),
    R3 = _{lando_req:lando_requirement{
                         fret_req:
                         fretment(unused,
                                  condition_info(_{condition: "regular",
                                                   regular_condition: CC3}),
                                  component_info(_{component: "comp1"}),
                                  unused,
                                  response_info(_{post_condition:PC3}))}},
    V1 = _{varname: "var1", usage: "Output"},
    V2 = _{varname: "var2", usage: "Internal"},
    V3 = _{varname: "var3", usage: "Input"},
    V4 = _{varname: "var4", usage: "Output"},
    connected_components([R1, R2, R3], [V1, V2, V3, V4], 0, C),
    assertion(C == [comp(0, "comp1", [R2, R1], ["var1", "var4"]),
                    comp(1, "comp1", [R3], [])
                   ]).

landoreq1(_{lando_req:lando_requirement{
                          fret_req:
                          fretment(
                              unused,
                              condition_info(_{condition: "regular",
                                               regular_condition: CC1}),
                              component_info(_{component: "comp1"}),
                              unused,
                              response_info(_{post_condition:PC1}))}}) :-
    % uses var1/Inp, var2/Int, var3/Inp, var4/Out
    CC1 = fretish(op(and(op(eq(term(ident("var3"), integer),
                               term(val(34), integer)), bool),
                         op(eq(term(ident("var2"), rocket_T),
                               term(val(87), rocket_T)), bool)), bool)),
    PC1 = fretish(op(or(op(eq(term(ident("var1"), bool),
                              term(lit(false), bool)), bool),
                        op(eq(term(ident("var4"), integer),
                              term(val(1), integer)), bool)),
                     bool)).

landoreq2(_{lando_req:lando_requirement{
                          fret_req:
                          fretment(
                              unused,
                              condition_info(_{condition: "regular",
                                               regular_condition: CC2}),
                              component_info(_{component: "comp1"}),
                              unused,
                              response_info(_{post_condition:PC2}))}}) :-
    % uses var3/Inp, var4/Out
    CC2 = fretish(op(eq(term(ident("var3"), integer),
                        term(val(33), integer)), bool)),
    PC2 = fretish(op(eq(term(ident("var4"), integer),
                        term(ident("var3"), integer)), bool)).

landoreq3(_{lando_req:lando_requirement{
                          fret_req:
                          fretment(
                              unused,
                              condition_info(_{condition: "regular",
                                               regular_condition: CC3}),
                              component_info(_{component: "comp1"}),
                              unused,
                              response_info(_{post_condition:PC3}))}}) :-
    % uses var3/Inp, var2/Int
    CC3 = fretish(op(eq(term(ident("var3"), integer),
                        term(val(33), integer)), bool)),
    PC3 = fretish(op(eq(term(ident("var2"), rocket_T),
                        term(val(87), rocket_T)), bool)).

landoreq4(_{lando_req:lando_requirement{
                          fret_req:
                          fretment(
                              unused,
                              condition_info(_{condition: "regular",
                                               regular_condition: CC4}),
                              component_info(_{component: "comp1"}),
                              unused,
                              response_info(_{post_condition:PC4}))}}) :-
    % uses var5/Out
    % ignored condition, response is... ineffective but ok
    CC4 = fretish(term(lit(true), bool), bool),
    PC4 = fretish(term(ident("var5"), bool)).

landoreq6(_{lando_req:lando_requirement{
                          fret_req:
                          fretment(
                              unused,
                              condition_info(_{condition: "regular",
                                               regular_condition: CC6}),
                              component_info(_{component: "comp1"}),
                              unused,
                              response_info(_{post_condition:PC6}))}}) :-
    % Uses var2/Internal
    % condition and response are both invalid FRETish, but ok as var refs.
    CC6 = fretish(term(ident("var2"), rocket_T)),
    PC6 = fretish(term(ident("var2"), rocket_T)).

test(five_rel_six_outvar_four_component, [nondet]) :-
    landoreq1(R1),
    landoreq2(R2),
    landoreq3(R3),
    landoreq4(R4),
    CC5 = fretish(op(term(lit(true), bool), bool)),
    PC5 = fretish(op(eq(term(ident("var5"), bool),
                        term(ident("var6"), bool)), bool)),
    R5 = _{lando_req:lando_requirement{
                         fret_req:
                         fretment(unused,
                                  condition_info(_{condition: "regular",
                                                   regular_condition: CC5}),
                                  component_info(_{component: "comp1"}),
                                  unused,
                                  response_info(_{post_condition:PC5}))}},
    landoreq6(R6),
    V1 = _{varname: "var1", usage: "Output"},
    V2 = _{varname: "var2", usage: "Internal"},
    V3 = _{varname: "var3", usage: "Input"},
    V4 = _{varname: "var4", usage: "Output"},
    V5 = _{varname: "var5", usage: "Output"},
    V6 = _{varname: "var6", usage: "Output"},
    connected_components([R1, R2, R3, R4, R5, R6],
                         [V1, V2, V3, V4, V5, V6], 0, C),
    assertion(C == [comp(0, "comp1", [R2, R1], ["var1", "var4"]),
                    comp(1, "comp1", [R3], []),
                    comp(2, "comp1", [R5, R4], ["var6", "var5"]),
                    comp(3, "comp1", [R6], [])
                   ]).

test(five_rel_six_outvar_two_component, [nondet]) :-
    landoreq1(R1),
    landoreq2(R2),
    landoreq3(R3),
    landoreq4(R4),
    CC5 = fretish(op(term(lit(true), bool), bool)),
    PC5 = fretish(op(eq(term(ident("var4"), bool),
                        term(ident("var5"), bool)), bool)),
    R5 = _{lando_req:lando_requirement{
                         fret_req:
                         fretment(unused,
                                  condition_info(_{condition: "regular",
                                                   regular_condition: CC5}),
                                  component_info(_{component: "comp1"}),
                                  unused,
                                  response_info(_{post_condition:PC5}))}},
    landoreq6(R6),
    V1 = _{varname: "var1", usage: "Output"},
    V2 = _{varname: "var2", usage: "Internal"},
    V3 = _{varname: "var3", usage: "Input"},
    V4 = _{varname: "var4", usage: "Output"},
    V5 = _{varname: "var5", usage: "Output"},
    V6 = _{varname: "var6", usage: "Output"},
    connected_components([R1, R2, R3, R4, R5, R6],
                         [V1, V2, V3, V4, V5, V6], 0, C),
    % n.b. the list of reqs for each component is metastable.  Also the vars list.
    assertion(C == [comp(0, "comp1", [R4, R5, R2, R1], ["var5", "var1", "var4"]),
                    comp(1, "comp1", [R3], []),
                    comp(2, "comp1", [R6], [])
                   ]).

test(five_rel_six_outvar_two_component_no_cond, [nondet]) :-
    landoreq1(R1),
    landoreq2(R2),
    landoreq3(R3),
    landoreq4(R4),
    PC5 = fretish(op(eq(term(ident("var4"), bool),
                        term(ident("var5"), bool)), bool)),
    R5 = _{lando_req:lando_requirement{
                         fret_req:
                         fretment(unused,
                                  condition_info(_{condition: "null"}),
                                  component_info(_{component: "comp1"}),
                                  unused,
                                  response_info(_{post_condition:PC5}))}},
    landoreq6(R6),
    V1 = _{varname: "var1", usage: "Output"},
    V2 = _{varname: "var2", usage: "Internal"},
    V3 = _{varname: "var3", usage: "Input"},
    V4 = _{varname: "var4", usage: "Output"},
    V5 = _{varname: "var5", usage: "Output"},
    V6 = _{varname: "var6", usage: "Output"},
    connected_components([R1, R2, R3, R4, R5, R6],
                         [V1, V2, V3, V4, V5, V6], 0, C),
    % n.b. the list of reqs for each component is metastable.  Also the vars list.
    assertion(C == [comp(0, "comp1", [R4, R5, R2, R1], ["var5", "var1", "var4"]),
                    comp(1, "comp1", [R3], []),
                    comp(2, "comp1", [R6], [])
                   ]).
