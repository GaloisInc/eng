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
    demand(outer, form(mem, crazy), _).
% KWQ: what about a demand that is successful but produces nothing v.s. a demand that causes an error?

test(genesis_engine, [nondet,
                      cleanup(restart_production)]) :-
    findall(T, demand(outer, form(mem, sample_tribble), T), Tribbles),
    expected_things([ (tribble(one), form(mem, sample_tribble)) ],
                    Tribbles).

test(unsatisfiable_demand_no_input, [nondet, fail]) :-
    demand(outer, form(mem, tribble), _).

test(single_consuming_engine, [nondet,
                               cleanup(restart_production)]) :-
    provide(coin, form(mem, money), Coin),
    findall(T, demand(outer, form(mem, tribble), T), Tribbles),
    expected_things([ (tribble(two), form(mem, tribble)) ],
                    Tribbles),
    Tribbles = [Tribble],
    same_production_series(Coin, Tribble).

test(single_consuming_engine_multiple_inputs, [nondet,
                                               cleanup(restart_production)]) :-
    provide(coin, form(mem, money), _Coin1),
    provide(coin, form(mem, money), _Coin2),
    provide(coin, form(mem, money), _Coin3),
    findall(T, demand(outer, form(mem, tribble), T), Tribbles),
    !,
    expected_things( [ (tribble(two), form(mem, tribble)),
                       (tribble(two), form(mem, tribble)),
                       (tribble(two), form(mem, tribble))
                     ],
                     Tribbles).

test(single_consuming_engine_multiple_inputs_repeated,
     [nondet,
      cleanup(restart_production)]) :-
    % Start with a Stock of three coins
    provide(coin, form(mem, money), _Coin1),
    provide(coin, form(mem, money), _Coin2),
    provide(coin, form(mem, money), _Coin3),
    % Three coins should allow the 3 demands producing a tribble each
    findall(T, demand(outer, form(mem, tribble), T), Tribbles),
    !,
    expected_things( [ (tribble(two), form(mem, tribble)),
                       (tribble(two), form(mem, tribble)),
                       (tribble(two), form(mem, tribble))
                     ],
                     Tribbles),
    % Now query again: this is the same demand and no new stock is available, so
    % this should not get any more tribbles. (engines are not re-run but
    % generated stock is available).
    findall(T, demand(outer, form(mem, tribble), T), NewTribbles),
    !,
    assertion([] == NewTribbles),
    % When we got Tribbles, we got trouble!
    demand(outer, form(mem, trouble), Trouble),
    % Trouble is associated with a Tribble
    member(thing(_, _, DID), Tribbles),
    assertion(thing(more, form(mem, trouble), DID) == Trouble),
    % But only one set of trouble for each tribble...
    findall(T, demand(outer, form(mem, trouble), T), Trouble2),
    expected_things( [ (trouble, form(mem, trouble)),
                       (trouble, form(mem, trouble))
                     ],
                     Trouble2),
    % and no more!
    ( demand(outer, form(mem, trouble), ExcessiveTrouble),
      format('demand unexpectedly succeeded with ~w~n', [ExcessiveTrouble])
    ; ExcessiveTrouble = []
    ),
    assertion([] == ExcessiveTrouble),
    % Unless we are a different demander.
    findall(T, demand(other_outer, form(mem, trouble), T), OtherTrouble),
    expected_things( [ (trouble, form(mem, trouble)),
                       (trouble, form(mem, trouble)),
                       (trouble, form(mem, trouble))
                     ],
                     OtherTrouble).


test(same_production,
     [nondet,
      cleanup(restart_production)]) :-
    provide(coin, form(mem, money), Coin1),
    provide(coin, form(mem, money), _Coin2),
    % Just demand a single tribble, using a single coin and running the engine a
    % single time.
    demand(outer, form(mem, tribble), Tribble1),
    expected_things( [ (tribble(two), form(mem, tribble))
                     ],
                     [Tribble1]),
    % Verify the tribble obtained is related to the coin provided
    same_production_series(Coin1, Tribble1),
    !,
    % Now query again for the same thing.  This will not return the first tribble
    % because it was already provided, but the engine can run to generate another
    % tribble using the second coin.
    findall(T, demand(outer, form(mem, tribble), T), NewTribbles),
    expected_things( [ (tribble(two), form(mem, tribble))
                     ],
                     NewTribbles),
    assertion([Tribble1] \= NewTribbles),
    % The second tribble is NOT associated with the first coin
    NewTribbles = [Tribble2],
    ( same_production_series(Tribble2, Coin1),
      format('unexpected: tribble2 associated with coin1~n'), !, fail
    ; true
    ),
    % And the two tribbles are not associated with each other
    ( same_production_series(Tribble2, Tribble1),
      format('unexpected: tribble2 associated with tribble1~n'), !, fail
    ; true
    ).
