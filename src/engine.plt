:- begin_tests(engine).
:- use_module(engine).
:- use_module(etest).

expected_things(Expected, Got) :-
    length(Expected, NumExpected),
    length(Got, NumGot),
    assertion(NumExpected == NumGot).
expected_things([], []).
expected_things([], TooMany) :- writeln(too_many), assertion([] == TooMany).
expected_things(More, []) :- writeln(want_more), assertion(More == []).
expected_things([(V,F)|More], [Thing|Things]) :-
    thing_form(Thing, TF),
    thing_value(Thing, TV),
    assertion((V, F) == (TV, TF)),
    expected_things(More, Things).


test(unsatisfiable_demand_no_engine, [nondet, fail]) :-
    demand(form(mem, crazy), _).
% KWQ: what about a demand that is successful but produces nothing v.s. a demand that causes an error?

test(genesis_engine, [nondet,
                      cleanup(restart_production)]) :-
    writeln(ge1),
    findall(T, demand(form(mem, sample_tribble), T), Tribbles),
    writeln(ge2),
    expected_things([ (tribble(one), form(mem, sample_tribble)) ],
                    Tribbles).

test(unsatisfiable_demand_no_input, [nondet, fail]) :-
    demand(form(mem, tribble), _).

test(single_consuming_engine, [nondet,
                               cleanup(restart_production)]) :-
    provide(coin, form(mem, money)),
    findall(T, demand(form(mem, tribble), T), Tribbles),
    expected_things([ (tribble(two), form(mem, tribble)) ],
                    Tribbles).

test(single_consuming_engine_multiple_inputs, [nondet,
                                               cleanup(restart_production)]) :-
    provide(coin, form(mem, money)),
    provide(coin, form(mem, money)),
    provide(coin, form(mem, money)),
    findall(T, demand(form(mem, tribble), T), Tribbles),
    expected_things( [ (tribble(two), form(mem, tribble)),
                       (tribble(two), form(mem, tribble)),
                       (tribble(two), form(mem, tribble))
                     ],
                     Tribbles).

test(single_consuming_engine_multiple_inputs_repeated,
     [nondet,
      cleanup(restart_production)]) :-
    provide(coin, form(mem, money)),
    provide(coin, form(mem, money)),
    provide(coin, form(mem, money)),
    findall(T, demand(form(mem, tribble), T), Tribbles),
    expected_things( [ (tribble(two), form(mem, tribble)),
                       (tribble(two), form(mem, tribble)),
                       (tribble(two), form(mem, tribble))
                     ],
                     Tribbles),
    % Now query again: this should get the previous set again, but not create
    % more because we didn't restart production.
    findall(T, demand(form(mem, tribble), T), NewTribbles),
    assertion(NewTribbles == Tribbles),
    % When we got Tribbles, we got trouble!
    demand(form(mem, trouble), Trouble),
    member(thing(_, _, DID), Tribbles),
    assertion(thing(more, form(mem, trouble), DID) == Trouble).
