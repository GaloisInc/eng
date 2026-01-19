:- use_module(datafmts/eqil).
:- use_module(datafmts/lando).
:- use_module(datafmts/frettish).
:- use_module(datafmts/ltl).
:- use_module(datafmts/cabal).
:- use_module(fret_kind2).
:- use_module(engine).
:- set_test_options([show_blocked(true),
                     output(on_failure)
                     % output(always)
                    ]).


test_all :- load_test_files([]), run_tests.
