%% This module contains test engine implementations.  This needs to be a separate
%% module because the engine callables need to be declared with a namespace and
%% engine.plt does not have a useable namespace.  Having the engine separate from
%% the top-level demand is useful additional testing anyhow.

:- module(etest, []).
:- use_module(engine).

:- engine(etest:peddler, form(mem, tribble), "seller.tribble").
:- engine(etest:peddler, form(mem, sample_tribble), "sample.tribble").

peddler(_, Form, [(tribble(one), Form, [])]) :-
    Form = form(mem, sample_tribble),
    % the first one is free
    true.
peddler(D, Form, [(tribble(two), Form, [Money])
                  ,(more, form(mem, trouble), [Money])  % tribbles are trouble as well!
                 ]) :-
    Form = form(mem, tribble),
    % One tribble costs one coin, single production run.
    demand(D, form(mem, money), Money).
