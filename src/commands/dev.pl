:- module(dev, [ dev_cmd/3, dev_focus/1, dev_help/1, dev_help/2 ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(strings)).
:- use_module(library(yall)).
:- use_module('../load.pl').
:- use_module('../englib').
:- use_module(exec_subcmds).

dev_focus("Development Engineering").

dev_help(Info) :-
    engfile_dir(EngDirV),
    exec_subcmd_help("dev", ExecHelpI),
    exec_spec_help(10, ExecInfo),
    [EngDir, ExecHelp, ExecSpecHelp] = [EngDirV, ExecHelpI, ExecInfo],
    Info = {|string(EngDir, ExecHelp, ExecSpecHelp)||
| Perform a DEVELOPMENT engineering task.
|
| Development tasks typically consist of running one or more user-defined
| operations (e.g. "make", "cargo", "sed") in the current working tree.
|
| Development information is specified in one or more .eng files
| in the {EngDir} directory of the project.  These files are typically in
| EQIL format (see the EQIL design document), and have the following structure
| (uppercase is user-specified input):
|
| {ExecHelp}
|
| Testing:
| --------
|
| For the test sub-command, a normal exec can be used, but there is additional
| EQIL specification recognized for tests:
|
|     test =
|       testcase =
|         TESTNAME =
|           type = TESTTYPE
|           runner = TESTRUNNER
|           ARG = ARG FOR TESTTYPE FOR THIS TEST
|       testrunner =
|         TESTRUNNER =
|           {ExecSpecHelp}
|
| There can be multiple testcase entries that can use the same testrunner, and
| the testrunner can have testtype-specific arguments that will be substituted
| in the testrunner exec when the ARG appears in curly-braces in the exec. The
| test will be considered to have failed if the SHELL commands return a non-zero
| value (and no commands are executed for that testrunner after a command has
| a non-zero status).
|
| The TESTTYPE must be one that ~eng~ knows and supports to properly supply the
| test type specific argument substitution, as well as any other functionality
| associated with that test type.  Known TESTTYPE values and their arguments:
|
|  TESTTYPE  Description           ARG       ARG value
|  --------  -----------           ---       ---------
|  EXE       Execute test app      TestName  TESTNAME for this test
|                                  Input     input data file to the test
|
|  KAT       Known Answer Testing  TestName  TESTNAME for this test
|                                  Input     input data file to the test
|                                  Output    where to put the test output file
|                                  Expected  file containing expected output (KA)
|
| The handling of "exec", "in dir", and "env vars" settings for a testrunner
| are handled similarly to the normal subcmd exec; see that help description
| for more information.
|}.

dev_help(SubCmd, Help) :- subcmd_help(SubCmd, Help).

dev_cmd(Context, [Cmd|Args], Sts) :-
    eng:key(dev, subcmd, Cmd), !,
    ( member('--help', Args), dev_subcmd_help(Cmd), Sts = 0
    ; dev_subcmd_do(Context, Cmd, Args, Sts)
    ), !.
dev_cmd(Context, [testlist|Args], Sts) :-
    dev_subcmd_do(Context, testlist, Args, Sts).
dev_cmd(Context, [Cmd|_], unknown(Cmd, invalid_subcmd(dev, Context, Cmd))).


subcmd_help(Cmd, CmdHelp) :-
    eng:key(dev, subcmd, Cmd, H),
    \+ member(Cmd, [ test, testlist ]),
    (is_list(H), H = [CmdHelp|_]
    ; \+ is_list(H), split_string(H, "\n", "", [CmdHelp|_])
    ).
subcmd_help(test, "Runs tests for this project") :-
    eng:key(dev, subcmd, test, testcase, _).
subcmd_help(testlist, "Lists tests for this project") :-
    eng:key(dev, subcmd, test, testcase, _).


dev_subcmd_help(Cmd) :-
    atom_string(CmdA, Cmd),
    eng:eng(dev, subcmd, CmdA, CmdHelp),
    format('Help for dev "~w" sub-command:~n~n~w~n', [ Cmd, CmdHelp ]).


dev_subcmd_do(Context, test, Args, Sts) :-
    % test operations are slightly more complex as described in the help above.
    !,
    run_tests(Context, Args, Cnt, Sts),
    (Sts = 0, !, print_message(info, tests_passed(Cnt))
    ; print_message(error, test_failures(Cnt, Sts))
    ).
dev_subcmd_do(Context, testlist, _Args, 0) :-
    !,
    list_tests(Context, Cnt),
    print_message(info, test_count(Cnt)).
dev_subcmd_do(Context, Cmd, Args, Sts) :-
    exec_subcmd_do(Context, dev, Cmd, Args, Sts),
    !.
dev_subcmd_do(_, Cmd, _Args, unknown(Cmd, unknown_dev_subcmd_do(Cmd))).


%% ----------------------------------------------------------------------

list_tests(_Context, Cnt) :-
    findall(T, list_test(T), TS),
    length(TS, Cnt).

prolog:message(test_count(N)) --> [ '~w defined primary tests' - [N]].

list_test(T) :-
    eng:key(dev, subcmd, test, testcase, T),
    eng:eng(dev, subcmd, test, testcase, T, type, TestType),
    eng:eng(dev, subcmd, test, testcase, T, runner, TestRunner),
    format('___ ~w __ ~w ~`_t ~w ___~77|~n', [TestType, T, TestRunner]).


run_tests(Context, Args, Cnt, NumFailures) :-
    findall(S, (eng:key(dev, subcmd, test, testcase, T),
                run_test(Context, T, Args, S),
                report_result(T, S)
               ),
            TestStatus),
    length(TestStatus, Cnt),
    foldl(plus, TestStatus, 0, NumFailures).

run_test(Context, TestName, Args, TestFailed) :-
    eng:eng(dev, subcmd, test, testcase, TestName, type, TestType),
    (run_test_type(Context, TestName, TestType, Args, TestFailed), !
    ; % Maybe there's no run_test_type defined below for this TestType!
     TestFailed = 1).
run_test(_Context, TestName, _Args, 1) :-
    \+ eng:key(dev, subcmd, test, testcase, TestName, type),
    print_message(error, test_missing_type(TestName)).

run_test_type(Context, TestName, "EXE", _Args, TestFailed) :-
    % arguments for an EXE test (optional)
    (eng:eng(dev, subcmd, test, testcase, TestName, 'Input', TestSource) ;
     TestSource = ""),
    !,
    ArgMap = ['TestName' = TestName,
              'Input' = TestSource
             ],
    % TODO: use input Args somehow?
    exec_test_runner(Context, ArgMap, TestName, TestFailed).

run_test_type(Context, TestName, "KAT", _Args, TestFailed) :-
    % arguments for a KAT test: all are *required*
    eng:eng(dev, subcmd, test, testcase, TestName, 'Input', TestSource),
    eng:eng(dev, subcmd, test, testcase, TestName, 'Output', TestOutput),
    eng:eng(dev, subcmd, test, testcase, TestName, 'Expected', TestExpected),
    !,
    ArgMap = ['TestName' = TestName,
              'Input' = TestSource,
              'Output' = TestOutput,
              'Expected' = TestExpected
             ],
    % TODO: use input Args somehow?
    exec_test_runner(Context, ArgMap, TestName, TestFailed).
run_test_type(_Context, TestName, "KAT", _Args, 1) :-
    % arguments for a KAT test: all are *required*
    \+ eng:key(dev, subcmd, test, testcase, TestName, 'Input'),
    print_message(error, test_argument_missing(TestName, "KAT", "Input")),
    fail.
run_test_type(_Context, TestName, "KAT", _Args, 1) :-
    % arguments for a KAT test: all are *required*
    \+ eng:key(dev, subcmd, test, testcase, TestName, 'Output'),
    print_message(error, test_argument_missing(TestName, "KAT", "Output")),
    fail.
run_test_type(_Context, TestName, "KAT", _Args, 1) :-
    % arguments for a KAT test: all are *required*
    \+ eng:key(dev, subcmd, test, testcase, TestName, 'Expected'),
    print_message(error, test_argument_missing(TestName, "KAT", "Expected")),
    fail.
run_test_type(_Context, _TestName, "KAT", _Args, 1) :- !.

run_test_type(_Context, TestName, TestType, _Args, 1) :-
    print_message(error, unknown_test_type(TestName, TestType)).

exec_test_runner(Context, ArgMap, TestName, TestFailed) :-
    eng:eng(dev, subcmd, test, testcase, TestName, runner, TestRunner),
    atom_string(TestR, TestRunner),
    exec_from_spec_at(Context, ArgMap, [dev, subcmd, test, testrunner, TestR],
                      TestFailed).
exec_test_runner(_, _, TestName, 1) :-
    \+ eng:key(dev, subcmd, test, testcase, TestName, runner),
    print_message(error, test_runner_not_specified(TestName)),
    report_result(TestName, 1),
    fail.


report_result(TestName, TestFailed) :-
    ( TestFailed = 0, !,
      format('____ ~w Test ~`_t passed ____~77|~n', [TestName])
    ; format('____ ~w Test ~`_t FAILED ~`#t FAILED ____~77|~n', [TestName])
    ).


%% ----------------------------------------------------------------------

prolog:message(interpolate_failure(ArgMap, TE)) -->
    [ 'Unable to substitute ~w into ~w~n' - [ ArgMap, TE ] ].
prolog:message(unknown_dev_subcmd_do(Cmd)) -->
    [ 'No known action for "dev" sub-command "~w"' - [ Cmd ] ].
prolog:message(tests_passed(NumTests)) -->
    [ 'All ~w tests passed.' - [ NumTests ] ].
prolog:message(test_failures(NumTests, NumFailures)) -->
    [ 'Failed ~w out of ~w tests.' - [ NumFailures, NumTests ] ].
prolog:message(unknown_test_type(TestName, TestType)) -->
    [ 'Test "~w" has an unknown test type: ~w' - [ TestName, TestType ] ].
prolog:message(unknown_runner_exec(TestName, TestRunner)) -->
    [ 'Test "~w" runner ~w does not specify how to execute the runner'
      - [ TestName, TestRunner ] ].
prolog:message(test_runner_not_specified(TestName)) -->
    [ 'Test "~w" did not specify a test runner' - [ TestName ] ].
prolog:message(test_runner_not_found(TestName, TestRunner)) -->
    [ 'Test "~w" specifies an unknown test runner: ~w' - [ TestName, TestRunner ] ].
prolog:message(test_argument_missing(TestName, TestType, ArgName)) -->
    [ 'Test "~w" missing argument ~w for test type ~w' -
      [ TestName, ArgName, TestType ] ].
prolog:message(test_missing_type(TestName)) -->
    [ 'Test "~w" did not specify the test type' - [ TestName ] ].
