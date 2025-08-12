%% The engine module is used to generate potentially heavy-weight things.  As
%% opposed to a normal query, the engine/s demand/2 may cause significant amounts
%% of processing, as the engine that produced the demanded item may may demands
%% of its own.  The engine(s) satisfying the demand may have choice points for
%% providing additional demanded items, but once generated, these items are added
%% to the general stock of items; subsequent demands will obtain the items
%% directly from stock and will not re-run the engine.
%%
%% General use:
%%
%% * demand(Form, Thing) will (multiply) return a Thing of type
%%   Form, possibly finding and running an engine to supply the Thing(s)
%%   (recursively).
%%
%% * engine(EnginePred, Form [, Name) will declare an engine that can supply Form
%%   things.  There may be multiple Engines that can provide a single Form; only
%%   a single engine will be chosen on the basis of the Weight of that Engine.
%%   An engine may have choice points for backtracking, but the engine will only
%%   be called a single time; once it is done producing things, it is shutdown.
%%
%% * restart_production will start things over again, emptying all stock of
%%   Things and resetting engines so they can run again.
%%
%% * provide(What, Form) is called to provide initial stock that can be demanded
%%   by engines to run.

:- module(engine, [ engine/2, engine/3,
                    weight/2,
                    demand/2,
                    thing_form/2, thing_value/2,
                    provide/2,
                    stock/3,  % used internally
                    same_production_series/2,
                    restart_production/0
                  ]).

:- dynamic engine_/2, engine_/3, weight/2, stock/3.

:- use_module(englib).

%! demand(+X:form, --D:thing) is nondet
%
% This predicate generates a demand for an engine to produce one or more items of
% the 'X' form, returning a thing with choice points for potential additional
% thing returns.
%
% Form is:
%
%  * form(file, formtype)
%    where formtype should describe the contents, not the extension.
%  * form(mem, formtype)
%
% thing is intended to be opaque: use thing_form/2 and thing_value/2.
%
% The demand is nondet and will introduce a choice point for each possible
% generated item.  The demand can also be satisfied by items already generated
% (which may be a choice point with a different demanded ID), but only one engine
% will be invoked.

demand(Form, thing(Val, Form, DID)) :-
    % If it has already been produced/provided and is in stock, return it.
    stock(DID, Val, Form).

demand(Form, Thing) :-
    \+ is_list(Form),
    find_engine_for_form(Form, E, Name),
    !,
    \+ engine_run_completed(_, E),
    % Get a new unique demand ID (do this before trying to satisfy the demand in
    % case that satisfaction generates demands of its own). Note: there may have
    % already been items of this form in stock, with a DID that might be
    % different from the demand ID we will generate here.  However, the cut after
    % find_engine_for_form above ensures that only ONE engine will be selected
    % for any particular form (and thus, it wil never be re-invoked for the same
    % Form).
    next_demand_id(DID),
    retractall(next_demand_id(DID)),
    DID = did(DIDNUM),
    succ(DIDNUM, NewDID),
    asserta(next_demand_id(did(NewDID))),
    % Record the engine fulfilling this demand
    %% asserta(engine_used(DID, E)),
    % At present, all demands are fulfilled immediately and synchronously.
    callable(E),
    print_message(informational, running(E, Name, DID)),
    call(E, DID, Form, Producing),
    length(Producing, NProduced),
    print_message(informational, finished(E, Name, DID, NProduced)),
    assertz(engine_run_completed(DID, E)),
    stock_production(DID, Producing),
    member(R, Producing),
    demand_result(DID, R, Thing),
    thing_form(Thing, Form).

:- dynamic engine_run_completed/2.

stock_production(DID, [(Val, Form, Used)|Produced]) :-
    produced(DID, Val, Form, Used),
    stock_production(DID, Produced).
stock_production(_, []).

demand_result(DID, (Val, Form, _), thing(Val, Form, DID)).

thing_form(thing(_, Form, _), Form).
thing_value(thing(Value, _, _), Value).

:- asserta(next_demand_id(did(1))).

find_engine_for_form(Form, E, EName) :-
    findall((W, E, N), ((engine_(E, Form, N) ; engine_(E, Form), N=""),
                        (weight(E, W) ; W = 100)),
            WES),
    sort(WES, [(_,E,EName)|_]).

prolog:message(running(E, Name, DID)) -->
    [ 'running ~w ~w for ~w...' - [Name, E, DID]].
prolog:message(finished(E, Name, DID, N)) -->
    [ 'finished ~w ~w engine run for ~w, producing ~w results.' - [Name, E, DID, N]].

%! engine(+X:callable, +X:form) is det.
%
% Defines a callable that can provide an output of type 'form' when that form is
% demanded.  An engine fact should be asserted by anything that can perform this
% type of generation.
%
% The callable will receive a demand element as the first argument and the
% requested form as the second element; any actual items it produces (of any
% form) should be declared with the produced/4 predicate and supplying this
% demand element.
%
% The callable should perform at least the following operations, in this order:
%
%   1. Issue demand/2 predicates for any inputs needed.
%
%   2. Obtain results of previous demands using stock/3 (which may have
%      multiple choice points).
%
%   3. Perform whatever actions are needed to generate the outputs.
%
%   4. Record all generated outputs via the produced/4 predicate.

engine(Callable, Form) :-
    assertz(engine_(Callable, Form)).
engine(Callable, Form, Name) :-
    assertz(engine_(Callable, Form, Name)).


%! provide(+O:any, +F:form) is det.
%
% This is called to add stock that is generally/initially available and is not
% produced by any engine.  This adds stock that can be used in engine demands.

provide(What, Form) :- produced(provided, What, Form, []).


%! produced(+D:demand, +O:any, +F:form, +:demand_list) is det.
%
% This is called internally to add stock produced by the engine that is running
% with demand D to declare that it produced an output O of for F, and used the
% demanded_list as input demands to create that output.

produced(DID, What, Form, Using) :-
    assertz(stock(DID, What, Form)),
    produced(DID, Using).

produced(_, []).
produced(DID, [U|Using]) :- assertz(consumed(DID, U)), produced(DID, Using).
produced(DID, Using) :-
    \+ is_list(Using),
    assertz(consumed(DID, Using)).

% KWQ: produced -> stock; created -> produced DONE

%! same_production_series(A:thing, B:thing) is det.
%
% This predicate is true if both items are produced as part of the same run: if
% both A and B were produced from a common/shared dependency.
%
% Effectively, this means that the DID of the productions match, or the DID of a
% demanded input matches, transitively.

same_production_series(thing(_, _, D1), thing(_, _, D2)) :-
    same_production(D1, D2).

%% same_production_series(stock(D1, _, _), stock(D2, _, _)) :-
%%     same_production(D1, D2).

same_production(D1, D1).
same_production(D1, D2) :- consumed(D1, D2).
same_production(D1, D2) :- consumed(D2, D1).
same_production(D1, D2) :- consumed(D1, D3), consumed(D2, D4),
                           same_production(D3, D4).


restart_production :-
    retractall(stock(_, _, _)),
    retractall(engine_run_completed(_,_)).
