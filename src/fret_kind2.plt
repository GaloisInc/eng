:- begin_tests(fret_kind2).
:- use_module(fret_kind2).
:- use_module(library(strings)).

test(empty_no_components, [nondet]) :-
    connected_components([], [], 0, C),
    assertion(C == []).

test(one_rel_one_component, [nondet]) :-
    JR1 = _{semantics:_{variables: []}},
    R1 = _{requirement:JR1,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    connected_components([R1], [], 0, C),
    assertion(C == [comp(0, "comp1", [R1], [])]).

test(one_rel_one_outvar_one_component, [nondet]) :-
    JR1 = _{semantics:_{variables: ["var1"]}},
    R1 = _{requirement:JR1,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    V1 = _{variable_name: "var1", idType: "Output"},
    connected_components([R1], [V1], 0, C),
    assertion(C == [comp(0, "comp1", [R1], ["var1"])]).

test(one_rel_four_outvar_one_component, [nondet]) :-
    JR1 = _{semantics:_{variables: ["var1", "var2", "var3", "var4"]}},
    R1 = _{requirement:JR1,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    V1 = _{variable_name: "var1", idType: "Output"},
    V2 = _{variable_name: "var2", idType: "Internal"},
    V3 = _{variable_name: "var3", idType: "Input"},
    V4 = _{variable_name: "var4", idType: "Output"},
    connected_components([R1], [V1, V2, V3, V4], 0, C),
    assertion(C == [comp(0, "comp1", [R1], ["var1", "var4"])]).

test(three_rel_four_outvar_two_component, [nondet]) :-
    JR1 = _{semantics:_{variables: ["var1", "var2", "var3", "var4"]}},
    R1 = _{requirement:JR1,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR2 = _{semantics:_{variables: ["var3", "var4"]}},
    R2 = _{requirement:JR2,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR3 = _{semantics:_{variables: ["var3", "var2"]}},
    R3 = _{requirement:JR3,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    V1 = _{variable_name: "var1", idType: "Output"},
    V2 = _{variable_name: "var2", idType: "Internal"},
    V3 = _{variable_name: "var3", idType: "Input"},
    V4 = _{variable_name: "var4", idType: "Output"},
    connected_components([R1, R2, R3], [V1, V2, V3, V4], 0, C),
    assertion(C == [comp(0, "comp1", [R2, R1], ["var1", "var4"]),
                    comp(1, "comp1", [R3], [])
                   ]).

test(five_rel_six_outvar_four_component, [nondet]) :-
    JR1 = _{semantics:_{variables: ["var1", "var2", "var3", "var4"]}},
    R1 = _{requirement:JR1,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR2 = _{semantics:_{variables: ["var3", "var4"]}},
    R2 = _{requirement:JR2,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR3 = _{semantics:_{variables: ["var3", "var2"]}},
    R3 = _{requirement:JR3,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR4 = _{semantics:_{variables: ["var5"]}},
    R4 = _{requirement:JR4,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR5 = _{semantics:_{variables: ["var6", "var5"]}},
    R5 = _{requirement:JR5,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR6 = _{semantics:_{variables: ["var2"]}},
    R6 = _{requirement:JR6,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    V1 = _{variable_name: "var1", idType: "Output"},
    V2 = _{variable_name: "var2", idType: "Internal"},
    V3 = _{variable_name: "var3", idType: "Input"},
    V4 = _{variable_name: "var4", idType: "Output"},
    V5 = _{variable_name: "var5", idType: "Output"},
    V6 = _{variable_name: "var6", idType: "Output"},
    connected_components([R1, R2, R3, R4, R5, R6],
                         [V1, V2, V3, V4, V5, V6], 0, C),
    assertion(C == [comp(0, "comp1", [R2, R1], ["var1", "var4"]),
                    comp(1, "comp1", [R3], []),
                    comp(2, "comp1", [R5, R4], ["var6", "var5"]),
                    comp(3, "comp1", [R6], [])
                   ]).

test(five_rel_six_outvar_two_component, [nondet]) :-
    JR1 = _{semantics:_{variables: ["var1", "var2", "var3", "var4"]}},
    R1 = _{requirement:JR1,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR2 = _{semantics:_{variables: ["var3", "var4"]}},
    R2 = _{requirement:JR2,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR3 = _{semantics:_{variables: ["var3", "var2"]}},
    R3 = _{requirement:JR3,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR4 = _{semantics:_{variables: ["var5"]}},
    R4 = _{requirement:JR4,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR5 = _{semantics:_{variables: ["var4", "var5"]}},
    R5 = _{requirement:JR5,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    JR6 = _{semantics:_{variables: ["var2"]}},
    R6 = _{requirement:JR6,
           lando_req:lando_requirement{
                         fret_req: fretment(unused, unused,
                                            component_info(_{component: "comp1"}),
                                            unused, unused)}},
    V1 = _{variable_name: "var1", idType: "Output"},
    V2 = _{variable_name: "var2", idType: "Internal"},
    V3 = _{variable_name: "var3", idType: "Input"},
    V4 = _{variable_name: "var4", idType: "Output"},
    V5 = _{variable_name: "var5", idType: "Output"},
    V6 = _{variable_name: "var6", idType: "Output"},
    connected_components([R1, R2, R3, R4, R5, R6],
                         [V1, V2, V3, V4, V5, V6], 0, C),
    % n.b. the list of reqs for each component is metastable.  Also the vars list.
    assertion(C == [comp(0, "comp1", [R4, R5, R2, R1], ["var5", "var1", "var4"]),
                    comp(1, "comp1", [R3], []),
                    comp(2, "comp1", [R6], [])
                   ]).
