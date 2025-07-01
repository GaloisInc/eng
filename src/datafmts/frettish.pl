% Parses the FRETish statements supported by the NASA Fret tool
% (https://github.com/NASA-SW-VnV/fret).

:- module(frettish, [ parse_fret/3, emit_fretish/2, emit_fretish/3,
                      fretment_vars/3
                    ]).

:- use_module('../englib').
:- use_module('../exprlang').

%% Parses a FRETish English requirement to a Fret structured requirement, using
%% the definitions and templates provided to enhance the structured requirement.
%
%  Returns: fretment(scope_info({scope:{type:},
%                               [,scope_mode:[, exclusive:bool, required:bool]]
%                               }, [SCOPE_VAR_NAMES]),
%                    condition_info({condition:,
%                                    pre_condition:,
%                                    qualifier_word:,
%                                    regular_condition:}),
%                    component_info({component:}),
%                    timing_info({timing:[,duration:|stop_condition:]},
%                                [TIMING_VAR_NAMES]),
%                    response_info({response:,
%                                   post_condition:}),
%
parse_fret(Context, English, FretMent) :-
    fretish_expr_langdef(LangDef),
    define_language(LangDef, _),
    %% get_dict(language, LangDef, Language),
    %% show_language(Language),

    string_chars(English, ECodes),
    enumerate(ECodes, Input),
    phrase(fretish(FretMent), Input, Remaining),
    !,
    ( Remaining == []
    -> true
    ; ( Remaining = [(_,'.')]
      -> true
      ; show_remaining(Context, Remaining)
      )
    ).

show_remaining(Context, [(P,C)|CS]) :-
    unenumerate(Chars, CS),
    string_chars(RStr, Chars),
    string_chars(SS, [C]),
    string_concat(SS, RStr, Str),
    format('Unexpected fretish extra not parsed @ ~w, offset ~w: "~w"~n',
           [ Context, P, Str ]).

unenumerate([], []).
unenumerate([C|OS], [(_,C)|CS]) :- unenumerate(OS, CS).


%% Emits a fretment(..) as a FRETish English string.
emit_fretish(Fretment, English) :-
    fretish_parts(Fretment, ScopeText, CondText, CompText, TimingText, ResponseText),
    format_str(English, '~w~wthe ~w shall ~w satisfy ~w.',
               [ ScopeText, CondText, CompText, TimingText, ResponseText ]).

%% Emits a fretment(..) as a FRETish English string, along with a dictionary of
%% the range of each element in the string as ranges:{scopeTextRange:RANGE,
%% conditionTextRange:RANGE, componentTextRange:RANGE, timingTextRange:RANGE,
%% responseTextRange:RANGE} where each RANGE is specified as an array of [start
%% character index, end character index]
emit_fretish(Fretment, English, Ranges) :-
    fretish_parts(Fretment, ScopeText, CondText, CompText, TimingText, ResponseText),
    format_str(English, '~w~wthe ~w shall ~w satisfy ~w.',
               [ ScopeText, CondText, CompText, TimingText, ResponseText ]),
    string_length(ScopeText, SLen), % empty or includes trailing space
    string_length(CondText, CndLen), % empty or includes trailing space
    string_length(CompText, CmpLen),
    string_length(TimingText, TmLen),
    string_length(ResponseText, RspLen),
    (SLen == 0 -> SEnd = 0 ; SEnd is SLen - 2),
    (CndLen == 0 -> CndEnd is SEnd; CndEnd is SLen + CndLen - 2),
    (CndEnd == 0 -> CompStart = 0 ; CompStart is CndEnd + 2),
    CompEnd is CompStart + CmpLen + 3,
    TimingStart is CompEnd + 8,
    TimingEnd is TimingStart + TmLen - 1,
    RespStart is TimingEnd + 2,
    RespEnd is RespStart + RspLen + 7,
    RS = ranges{ conditionTextRange: [SLen, CndEnd],
                 componentTextRange: [CompStart, CompEnd],
                 timingTextRange: [TimingStart, TimingEnd],
                 responseTextRange: [RespStart, RespEnd]
               },
    (SLen == 0 -> Ranges = RS
    ; put_dict(RS, ranges{ scopeTextRange: [0, SEnd] }, Ranges)
    ).

%% ----------------------------------------------------------------------
%% Emitting
%
% Note that the Fretment AST elements can be emitted in any number of
% textual forms; this code is for regenerating FRETish from the AST.

fretish_parts(Fretment, ScopeText, CondText, CompText, TimingText, ResponseText) :-
    Fretment = fretment(scope_info(Scope, _),
                        condition_info(Condition),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Response)),
    scope_fretish(Scope, ScopeText),
    condition_fretish(Condition, CondText),
    component_fretish(Comp, CompText),
    timing_fretish(Timing, TimingText),
    response_fretish(Response, ResponseText).

scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, ST),
    member(ST, ["after", "before"]),
    get_dict(scope_mode, Scope, mode(M)),
    !,
    format_str(ScopeText, '~w ~w ', [ST, M]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, ST),
    member(ST, ["after", "before"]),
    !,
    get_dict(scope_mode, Scope, E),
    fretish_expr_langdef(LangDef),
    get_dict(language, LangDef, Language),
    emit_expr(Language, E, T),
    format_str(ScopeText, '~w ~w ', [ST, T]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, "in"),
    get_dict(scope_mode, Scope, mode(M)),
    !,
    format_str(ScopeText, 'in ~w ', [M]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, "in"),
    get_dict(scope_mode, Scope, E),
    !,
    fretish_expr_langdef(LangDef),
    get_dict(language, LangDef, Language),
    emit_expr(Language, E, T),
    format_str(ScopeText, 'while ~w ', [T]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, "notin"),
    get_dict(scope_mode, Scope, mode(M)),
    !,
    format_str(ScopeText, 'unless in ~w ', [M]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, "notin"),
    get_dict(scope_mode, Scope, E),
    !,
    fretish_expr_langdef(LangDef),
    get_dict(language, LangDef, Language),
    emit_expr(Language, E, T),
    format_str(ScopeText, 'except while ~w ', [T]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, "onlyIn"),
    get_dict(scope_mode, Scope, mode(M)),
    !,
    format_str(ScopeText, 'only in ~w ', [M]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, "onlyAfter"),
    get_dict(scope_mode, Scope, mode(M)),
    !,
    format_str(ScopeText, 'only after ~w ', [M]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, "onlyAfter"),
    !,
    get_dict(scope_mode, Scope, E),
    fretish_expr_langdef(LangDef),
    get_dict(language, LangDef, Language),
    emit_expr(Language, E, T),
    format_str(ScopeText, 'only after ~w ', [T]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, "onlyBefore"),
    get_dict(scope_mode, Scope, mode(M)),
    !,
    format_str(ScopeText, 'only before ~w ', [M]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, "onlyBefore"),
    !,
    get_dict(scope_mode, Scope, E),
    emit_fretish(E, T),
    format_str(ScopeText, 'only before ~w ', [T]).
scope_fretish(Scope, "") :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, null), !.
scope_fretish(Scope, bad) :-
    print_message(error, bad_scope_encoding(Scope)), !, fail.

prolog:message(bad_scope_encoding(S)) -->
    [ 'Cannot convert invalid Scope to FRETish: ~w' - [S] ].

condition_fretish(Condition, "") :- get_dict(condition, Condition, "null"), !.
condition_fretish(Condition, CondText) :-
    fretish_expr_langdef(LangDef),
    get_dict(language, LangDef, Language),
    get_dict(qualifier_word, Condition, QW),
    get_dict(regular_condition, Condition, CE),
    emit_expr(Language, CE, RC),
    format_str(CondText, '~w ~w ', [ QW, RC ]).

component_fretish(Comp, CompText) :-
    get_dict(component, Comp, CompText).

timing_fretish(Timing, "at the next timepoint") :- get_dict(timing, Timing, "next"), !.
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "after"), !,
                                get_dict(duration, Timing, D),
                                (D == 1 -> U = tick ; U = ticks),
                                format_str(Text, 'after ~w ~w', [D, U]).
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "before"), !,
                                get_dict(stop_condition, Timing, C),
                                fretish_expr_langdef(LangDef),
                                get_dict(language, LangDef, Language),
                                emit_expr(Language, C, CF),
                                format_str(Text, 'before ~w', [CF]).
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "for"), !,
                                get_dict(duration, Timing, D),
                                (D == 1 -> U = tick ; U = ticks),
                                format_str(Text, 'for ~w ~w', [D, U]).
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "until"), !,
                                get_dict(stop_condition, Timing, C),
                                fretish_expr_langdef(LangDef),
                                get_dict(language, LangDef, Language),
                                emit_expr(Language, C, CF),
                                format_str(Text, 'until ~w', [CF]).
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "within"), !,
                                get_dict(duration, Timing, D),
                                (D == 1 -> U = tick ; U = ticks),
                                format_str(Text, 'within ~w ~w', [D, U]).
%% timing_fretish(Timing, "") :- get_dict(timing, Timing, "always"), !.
timing_fretish(Timing, Text) :- get_dict(timing, Timing, Text).

response_fretish(Response, ResponseText) :-
    fretish_expr_langdef(LangDef),
    get_dict(language, LangDef, Language),
    get_dict(post_condition, Response, RE),
    emit_expr(Language, RE, ResponseText).


%% ----------------------------------------------------------------------
%% Parsing
%
% see FRET: fret/fret-electron/app/parser/Requirement.g4
%           fret/fret-electron/app/parser/SemanticsAnalyzer.js

fretishHelp("
Specify a FRETish statement like one of:
  [SCOPE] [CONDITION] [the] COMPONENT shall TIMING satisfy RESPONSES
  [SCOPE] [CONDITION] shall [the] COMPONENT TIMING satisfy RESPONSES

Note: use help! for any of the capitalized sections above to get more
information on specifying that portion of the FRETish statement.
").


fretment_vars(scope, fretment(scope_info(_, vars(VS)), _, _, _, _), VS).
fretment_vars(condition, fretment(_, condition_info(_{condition:"null"}), _, _, _), []) :- !.
fretment_vars(condition, fretment(_, condition_info(C), _, _, _), VS) :-
    get_dict(regular_condition, C, Expr),
    fretish_expr_langdef(LangDef),
    get_dict(language, LangDef, Language),
    extract_vars(Language, Expr, VS).
fretment_vars(timing, fretment(_, _, _, timing_info(_, vars(VS)), _), VS).
fretment_vars(response, fretment(_, _, _, _, response_info(R)), VS) :-
    get_dict(post_condition, R, Expr),
    fretish_expr_langdef(LangDef),
    get_dict(language, LangDef, Language),
    extract_vars(Language, Expr, VS).


% scope conditions component shall timing responses
fretish(fretment(scope_info(Scope, vars(ScopeVars)),
                 condition_info(Condition),
                 component_info(Comp),
                 timing_info(Timing, vars(TimingVars)),
                 response_info(Responses)
                )) -->
    { initial_gamma(Env) },
    intro(Env, Scope, ScopeVars, Condition, Env1),
    target(Comp),
    !,
    timing(Env1, Timing, TimingVars, Env2),
    %% {format('....timing: ~w~n', [Timing])},
    lexeme(tok(satisfy)),
    !,
    ( responses(Env2, Responses)
    ; word(EW, EP), { print_message(error, bad_response_text(EW, EP)) }
    ).


fretish(help) --> tok(help), [(_, '!')], !,
                  { print_message(help, fretish_help), fail }.
fretish(invalid) --> any(40, T, P), !,
                     { print_message(error, bad_fretish(T, P)), fail }.

prolog:message(fretish_help) --> { fretishHelp(H), scopeHelp(SH) },
                                 [ '~w~w' - [H, SH] ].
prolog:message(bad_fretish(T, P)) --> { fretishHelp(H) },
                                [ 'Bad FRETish statement @ ~w: ~w~n~w' - [P, T, H] ].

intro(Env, Scope, ScopeVars, Condition, OutEnv) -->
    scope(Env, Scope, ScopeVars, Env1),
    %% {format('....scope: ~w~n', [Scope])},
    conditions(Env1, Condition, OutEnv).

target(Comp) -->
    component(Comp),
    %% {format('....component: ~w~n', [Comp])},
    lexeme(tok(shall)),
    !.
target(Comp) -->
    lexeme(tok(shall)),
    !,
    component(Comp).
    %% {format('....component: ~w~n', [Comp])},
target(fail) -->
    any(20, EW, EP), { print_message(error, bad_component(EW, EP)), !, fail }.

prolog:message(bad_component(EW, EP)) -->
    { componentHelp(H) },
    [ 'Invalid FRETish Component specification at character ~w: ~w~n~w'
      - [ EP, EW, H ] ].
prolog:message(bad_timing(EW, EP)) -->
    { timingHelp(H) },
    [ 'Invalid FRETish timing specification at character ~w: ~w~n~w'
      - [ EP, EW, H ] ].
prolog:message(bad_response_text(EW, EP)) -->
    [ 'Invalid FRETish Response specification at character ~w: ~w~n'
      - [ EP, EW ] ].

% --------------------------------------------------

% FRET semantics handles "globally" and "strictly", but these are not supported
% by the FRET parser, so they are not implemented.  The word "strictly" is the
% only way that exclusive: can be true (so it is never true).

scopeHelp("
| Scope Type | FRETish               |
|------------+-----------------------|
| null       |                       |
|------------+-----------------------|
| in         | in MODE               |
|            | if in MODE            |
|            | when in MODE          |
|            | during MODE           |
|            | while COND            |
|------------+-----------------------|
| notin      | except in MODE        |
|            | except if in MODE     |
|            | except when in MODE   |
|            | except during MODE    |
|            | if not in MODE        |
|            | when not in MODE      |
|            | unless in MODE        |
|            | except while COND     |
|------------+-----------------------|
| onlyIn     | only in MODE          |
|            | only if in MODE       |
|            | only when in MODE     |
|            | only during MODE      |
|------------+-----------------------|
| after      | after MODE/COND       |
|------------+-----------------------|
| onlyAfter  | only after MODE/COND  |
|------------+-----------------------|
| before     | before MODE/COND      |
|------------+-----------------------|
| onlyBefore | only before MODE/COND |
|------------+-----------------------|

MODE is:
    NAME
    NAME mode
    mode NAME

COND is a boolean expression
").

scope(Env, Scope, Vars, OutEnv) --> scope_(Env, Scope, Vars, OutEnv), opt_comma, !.
scope(Env, _{scope:_{type: null}}, [], Env) --> [].

scope_(Env, _{scope:_{type: "after", exclusive: false, required: false},
         scope_mode:Mode}, Vars, OutEnv) -->
    tok(after),
    scope_mode(allow_expr, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "onlyAfter",exclusive: false, required: false},
         scope_mode:Mode}, Vars, OutEnv) -->
    tok(only), lexeme(tok(after)),
    scope_mode(allow_expr, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "before",exclusive: false, required: false},
         scope_mode:Mode}, Vars, OutEnv) -->
    tok(before),
    scope_mode(allow_expr, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "onlyBefore",exclusive: false, required: false},
         scope_mode:Mode}, Vars, OutEnv) -->
    tok(only), lexeme(tok(before)),
    scope_mode(allow_expr, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "in"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(during),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "notin"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(except), lexeme(tok(during)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "in"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(in),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "in"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(when), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "in"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(if), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "notin"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(except), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "notin"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(except), lexeme(tok(if)), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "notin"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(except), lexeme(tok(when)), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "onlyIn"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(only), lexeme(tok(during)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "onlyIn"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(only), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "onlyIn"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(only), lexeme(tok(if)), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "onlyIn"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(only), lexeme(tok(when)), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "in"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(while),
    scope_mode(allow_expr, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "notin"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(except), lexeme(tok(while)),
    scope_mode(allow_expr, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "notin"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(if), lexeme(tok(not)), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "notin"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(when), lexeme(tok(not)), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).
scope_(Env, _{scope:_{type: "notin"}, scope_mode:Mode}, Vars, OutEnv) -->
    tok(unless), lexeme(tok(in)),
    scope_mode(mode_only, Env, Mode, Vars, OutEnv).

% scope_mode: mode WORD | WORD mode | WORD   % KWQ: make vars for word (state = val)?
scope_mode(_, Env, mode(Mode), [Mode ⦂ bool], OutEnv) -->
    lexeme(tok(mode)),
    lexeme(word, Mode),
    !,
    fresh_var(Env, Mode, mode, OutEnv, _V).
scope_mode(_, Env, mode(Mode), [Mode ⦂ bool], OutEnv) -->
    lexeme(word, Mode),
    lexeme(tok(mode)),
    !,
    fresh_var(Env, Mode, mode, OutEnv, _V).
scope_mode(allow_expr, Env, E, Vars, OutEnv) -->
    bool_expr(Env, E, OutEnv), !, % n.b. this could also parse a "mode" as var(_)
    { fretish_expr_langdef(LangDef),
      get_dict(language, LangDef, Language),
      extract_vars(Language, E, Vars)
    }.
scope_mode(_, Env, mode(Mode), [Mode ⦂ bool], OutEnv) -->
    lexeme(word, Mode),
    fresh_var(Env, Mode, mode, OutEnv, _V).
scope_mode(_, Env, bad, [], Env) -->
    lexeme(any(20, I, P)), { print_message(error, bad_scope_mode(I, P)), !, fail }.


% --------------------------------------------------

conditionHelp("
Specify a condition as one or more qualified boolean expressions
that specify the set of input values for this statement to hold.

                                         +-----repeat if desired--+
                                         |                        |
                                         v                        |
| start | QUAL     | PRECOND           | separator | QUAL | PRECOND | end |
|-------+----------+-------------------+-----------+------+---------+-----|
|       | upon     | BOOLEXPR          | ,         | QUAL | PRECOND |     |
| [and] | whenever | BOOLEXPR is true  | [,] and   |      |         | ,   |
|       | when     | BOOLEXPR is false | [,] or    |      |         |     |
|       | unless   |                   |           |      |         |     |
|       | where    |                   |           |      |         |     |
|       | if       |                   |           |      |         |     |

The 'and' and 'or' separators have equal priority and all conditions are
joined as right-associative.  More explicit control of the condition can
be achieved by using a single PRECOND with appropriate parentheses to
control order of evaluation.
").

prolog:message(condition_help) -->
    { conditionHelp(H) },
    [ 'Help for specifying a FRETish condition:~w' - [H] ].

conditions(Env, fail, Env) --> tok(help), [(_, '!')], !,
                               { print_message(help, condition_help), fail }.

conditions(Env, ReqCond, OutEnv) --> tok(and),
                                     cond_(Env, C, OutEnv), !,
                                     { set_cond(C, ReqCond) }.
conditions(Env, ReqCond, OutEnv) --> cond_(Env, C, OutEnv),
                                     !,
                                     { set_cond(C, ReqCond) }.
conditions(Env, _{condition:"null"}, Env) --> [].

set_cond(C, ReqCond) :-
    (get_dict(qualifier_word, C, "whenever") -> CND = "holding" ; CND = "regular"),
    put_dict(C, _{condition:CND}, ReqCond).


cond_(Env, C, OutEnv) --> qcond1_(Env, C0, Env1),
                          opt_comma,
                          qcond2_(Env1, C0, C, OutEnv),
                          opt_comma.
cond_(Env, C, OutEnv) --> qcond1_(Env, C, OutEnv),
                          opt_comma.

qcond1_(Env, C, OutEnv) -->
    lexeme(tok(unless)),
    !,
    lexeme(precond, Env, E, OutEnv),
    { !, qcond1_false_("unless",E,C)}.
qcond1_(Env, C, OutEnv) -->
    lexeme(qualifier, Q),
    !,  % green cut
    qcond1__(Env, C, Q, OutEnv).

qcond1__(Env, C, Q, OutEnv) -->
    lexeme(precond, Env, E, OutEnv),
    !,  % green cut
    qcond1___(C, Q, E).

qcond1___(C, Q, E) --> lexeme(tok(is)), lexeme(tok(true)), !,  % green cut
                       { qcond1_true_(Q,E,C) }.
qcond1___(C, Q, E) --> lexeme(tok(is)), lexeme(tok(false)), !,  % green cut
                       { qcond1_false_(Q,E,C) }.
qcond1___(C, Q, E) --> { qcond1_true_(Q,E,C) }.

qcond1_true_(Q,E, _{ qualifier_word:Q,
                     pre_condition: E,
                     regular_condition: E
                   }).

qcond1_false_(Q,E, _{ qualifier_word:Q,
                      pre_condition: not(E),
                      regular_condition: not(E)
                    }).

qcond2_(Env, C0, C, OutEnv) --> tok(and), qcond2_and_(Env, C0, C, OutEnv).
qcond2_(Env, C0, C, OutEnv) -->
    tok(or),
    qcond1_(Env, C1, OutEnv),
    { get_dict(regular_condition, C0, C0P),
      get_dict(regular_condition, C1, C1P),
      get_dict(qualifier_word, C1, C1QW),
      C = _{ qualifier_word: C1QW,  % XXX: always just uses *last* qualifier?!
             pre_condition: or(C0P, C1P),
             regular_condition: or(C0P, C1P)
           }
    }.
qcond2_(Env, C0, C, OutEnv) --> qcond2_and_(Env, C0, C, OutEnv).
qcond2_and_(Env, C0, C, OutEnv) -->
    qcond1_(Env, C1, OutEnv),
    { get_dict(regular_condition, C0, C0P),
      get_dict(regular_condition, C1, C1P),
      get_dict(qualifier_word, C1, C1QW),
      C = _{ qualifier_word: C1QW,  % XXX: always just uses *last* qualifier?!
             pre_condition: and(C0P, C1P),
             regular_condition: and(C0P, C1P)
           }
    }.

qualifier("upon") --> tok(upon).
qualifier("whenever") --> tok(whenever).
qualifier("when") --> tok(when).
qualifier("unless") --> tok(unless).
qualifier("where") --> tok(where).
qualifier("if") --> tok(if).

precond(Env, E, OutEnv) --> bool_expr(Env, E, OutEnv), !.  % green cut for perf.

% --------------------------------------------------

componentHelp("
A FRETish component is a regular name (as an identifier: no spaces
or unusual characters), optionally preceeded by 'the'.

Note that FRETish statements and associated variables are segregated
by the component: different components will be evaluated entirely
separately.
").

prolog:message(component_help) -->
    { componentHelp(H) },
    [ 'Help for specifying a FRETish component:~w' - [H] ].

component(fail) --> tok(help), [(_, '!')], !,
                    { print_message(help, component_help), fail }.
component(_{component: Comp}) --> tok(the), !, lexeme(word, Comp).
component(_{component: Comp}) --> word(Comp).

% --------------------------------------------------

timingHelp("
| Effect      | FRETish                |
|-------------+------------------------|
| always      |                        |
|             | always                 |
|-------------+------------------------|
| after       | after DURATION         |
|-------------+------------------------|
| before      | before COND            |
|-------------+------------------------|
| immediately | immediately            |
|             | initially              |
|             | at the first timepoint |
|             | at the same timepoint  |
|-------------+------------------------|
| finally     | finally                |
|             | at the last timepoint  |
|-------------+------------------------|
| next        | at the next timepoint  |
|-------------+------------------------|
| for         | for DURATION           |
|-------------+------------------------|
| eventually  | eventually             |
|-------------+------------------------|
| never       | never                  |
|-------------+------------------------|
| until       | until COND             |
|-------------+------------------------|
| within      | within DURATION        |
|-------------+------------------------|

DURATION is a number followed by a TIMEUNIT.  The TIMEUNIT is a placeholder and
does not perform any scaling of the time factor.  Valid TIMEUNIT words are: tick,
microsecond, millisecond, second, minute, hour, and the plural of those words.

COND is a boolean expression.
").

prolog:message(timing_help) -->
    { timingHelp(H) },
    [ 'Help for specifying a FRETish timing phrase:~w' - [H] ].

timing(Env, fail, [], Env) --> lexeme(tok(help)), [(_, '!')], !,
                               { print_message(help, timing_help), fail }.
timing(Env, _{ timing: "always"}, [], Env) --> lexeme(tok(always)).
timing(Env, _{ timing: "after", duration: Duration }, [], Env) --> lexeme(tok(after)),
                                                                   duration_lower(Duration).
timing(Env, _{ timing: "before", stop_condition: Cond }, Vars, NewEnv) -->
    lexeme(tok(before)),
    bool_expr(Env, Cond, NewEnv),
    { fretish_expr_langdef(LangDef),
      get_dict(language, LangDef, Language),
      extract_vars(Language, Cond, Vars)
    }.
timing(Env, _{ timing: "immediately" }, [], Env) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(tok(first)), lexeme(tok(timepoint)).
timing(Env, _{ timing: "finally" }, [], Env) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(tok(last)), lexeme(tok(timepoint)).
timing(Env, _{ timing: "next" }, [], Env) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(tok(next)), lexeme(tok(timepoint)).
timing(Env, _{ timing: "immediately" }, [], Env) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(tok(same)), lexeme(tok(timepoint)).
timing(Env, _{ timing: "for", duration: Duration }, [], Env) --> lexeme(tok(for)),
                                                                 duration_upper(Duration).
timing(Env, _{ timing: "immediately" }, [], Env) --> lexeme(tok(immediately)).
timing(Env, _{ timing: "eventually" }, [], Env) --> lexeme(tok(eventually)).
timing(Env, _{ timing: "immediately" }, [], Env) --> lexeme(tok(initially)).
timing(Env, _{ timing: "finally" }, [], Env) --> lexeme(tok(finally)).
timing(Env, _{ timing: "never" }, [], Env) --> lexeme(tok(never)).
timing(Env, _{ timing: "until", stop_condition: Cond }, Vars, NewEnv) -->
    lexeme(tok(until)),
    bool_expr(Env, Cond, NewEnv),
    { fretish_expr_langdef(LangDef),
      get_dict(language, LangDef, Language),
      extract_vars(Language, Cond, Vars)
    }.
timing(Env, _{ timing: "within", duration: Duration }, [], Env) -->
    lexeme(tok(within)),
    duration_upper(Duration).
timing(Env, fail, [], Env) --> any(20, T, P),
                               { print_message(error, bad_timing(T, P)), !, fail }.

duration_lower(D) --> duration_upper(D).
duration_upper(D) --> lexeme(num, Dur),
                      lexeme(frettish:timeunit),
                      {
                          % ensure JSON outputs numbers as a string (because
                          % that's how FRET does it) by adding a trailing space
                          % which prevents it from looking like an integer to
                          % the JSON conversion.
                          format_str(D, "~w ", [Dur])
                      }.

timeunit --> lexeme(tok(ticks)).
timeunit --> lexeme(tok(tick)).
timeunit --> lexeme(tok(hours)).
timeunit --> lexeme(tok(hour)).
timeunit --> lexeme(tok(minutes)).
timeunit --> lexeme(tok(minute)).
timeunit --> lexeme(tok(seconds)).
timeunit --> lexeme(tok(second)).
timeunit --> lexeme(tok(milliseconds)).
timeunit --> lexeme(tok(millisecond)).
timeunit --> lexeme(tok(microseconds)).
timeunit --> lexeme(tok(microsecond)).

% --------------------------------------------------

responsesHelp("
Specify the responses as one or more boolean expressions
that specify the set of OUTPUT values that should hold
when this statement is effective.

Note that this is effectively 'scope+cond => responses', so if scope+cond is
true, responses must be true, but if scope+cond is false, the overall FRETish
statement is true.  Thus it may be possible (although confusing) to see a
realizability conflict with a FRETish statement whose scope+cond is false and
therefore whose responses portion is NOT true.
").

prolog:message(responses_help) -->
    { responsesHelp(H) },
    [ 'Help for specifying a FRETish responses phrase:~w' - [H] ].

responses(_, fail) --> lexeme(tok(help)), [(_, '!')], !,
                       { print_message(help, responses_help), fail }.
responses(Env, _{response: "satisfaction", post_condition: EP}) -->
    postcond(Env, EP).

postcond(Env, E) --> bool_expr(Env, E, _FinalEnv).

% --------------------------------------------------
%
% Boolean and numeric expressions.  The text forms are FRETtish specific,
% although they are very similar to other representations.  The parsing converts
% to a fairly generic AST format that could be serialized into other forms as
% well.

bool_exprHelp("
Boolean expressions:

  | FRETish expr                   | Meaning                   |
  |--------------------------------+---------------------------|
  | true                           | literal true              |
  | false                          | literal false             |
  | ! EXPR                         | invert                    |
  | ~ EXPR                         | invert                    |
  | EXPR & EXPR                    | conjunction               |
  | EXPR <PIPE> EXPR               | disjunction               |
  | EXPR xor EXPR                  | exclusive-or              |
  | if EXPR then EXPR              | implication               |
  | EXPR -> EXPR                   | implication               |
  | EXPR => EXPR                   | implication               |
  | EXPR <-> EXPR                  | biconditional equivalence |
  | EXPR <=> EXPR                  | biconditional equivalence |
  | (EXPR)                         | grouping                  |
  | NUMEXPR RELOP NUMEXPR          | numeric predicate         |
  | IDENT( BOOL_OR_NUMEXPR [,...]) | function call             |

Numeric expressions (NUMEXPR):
  |                              |                       |
  | FRETish expr                 | Meaning               |
  |------------------------------+-----------------------|
  | NUMBERS                      | literal numeric value |
  | NUMEXPR ^ NUMEXPR            | raise to the power    |
  | - NUMEXPR                    | negation              |
  | NUMEXPR + NUMEXPR            | addition              |
  | NUMEXPR - NUMEXPR            | subtraction           |
  | NUMEXPR * NUMEXPR            | multiplication        |
  | NUMEXPR / NUMEXPR            | whole division        |
  | NUMEXPR mod NUMEXPR          | remainder             |
  | (NUMEXPR)                    | grouping              |
  | IDENT(BOOL_OR_NUMEXPR [,...] | function call         |

Known functions:

  | Function                     | Meaning                                 |
  |------------------------------+-----------------------------------------|
  | occurred(number, BOOL_EXPR)  | The expression was true at least once   |
  |                              | in the period from the nth-previous     |
  |                              | tick through the current tick.          |
  |------------------------------+-----------------------------------------|
  | persisted(number, BOOL_EXPR) | The expression has been constantly true |
  |                              | in the period from the nth-previous     |
  |                              | tick through the current tick.          |
  |------------------------------+-----------------------------------------|

Note that all boolean and numeric expressions are strictly left associative
except for explicit sub-expressions in parentheses; there is no operator
precedence.
").


% These ops are exported from exprlang, but apparently their precedence is lost
% (causing →(⦂(if, bool),a) instead of ⦂(if, →(bool,a)) as expected).  Redeclare
% their precedence here.
:- op(760, yfx, ⦂).
:- op(750, xfy, →).

fretish_expr_langdef(
    langdef{
        language: fretish_expr,
        types: [ number, bool ],
        atoms: [ lit, num ],
        variable_ref: [ ident ],
        phrases:
        [ term(num ⦂ number, num, [term(num(N), number), T]>>fmt_str(T, '~w', [N])),
          term(lit ⦂ bool, [true]>>word(true), emit_simple_term(lit)),
          term(lit ⦂ bool, [false]>>word(false), emit_simple_term(lit)),
          term(ident ⦂ a, word, emit_simple_term(ident)),
          expop(and ⦂ bool → bool → bool, infix(chrs('&')), emit_infix("&")),
          expop(or ⦂ bool → bool → bool, infix(chrs('|')), emit_infix("|")),
          expop(exor ⦂ bool → bool → bool, infix(word(xor)), emit_infix("xor")),
          expop(neq ⦂ a → a → bool, infix(chrs('!=')), emit_infix("!=")),
          expop(neg ⦂ number → number, [[]>>lexeme(chrs('-')), subexpr],
                [_,[A],T]>>fmt_str(T, '(-~w)', [A])),
          expop(not ⦂ bool → bool, [[]>>lexeme(chrs('!')), subexpr],
                [_,[A],T]>>fmt_str(T, '(! ~w)', [A])),
          expop(eq ⦂ a → a → bool, infix(chrs('=')), emit_infix("=")),
          expop(geq ⦂ a → a → bool, infix(chrs('>=')), emit_infix(">=")),
          expop(leq ⦂ a → a → bool, infix(chrs('<=')), emit_infix("<=")),
          expop(gt ⦂ a → a → bool, infix(chrs('>')), emit_infix(">")),
          expop(lt ⦂ a → a → bool, infix(chrs('<')), emit_infix("<")),
          expop(add ⦂ number → number → number, infix(chrs('+')), emit_infix("+")),
          expop(sub ⦂ number → number → number, infix(chrs('-')), emit_infix("-")),
          expop(mul ⦂ number → number → number, infix(chrs('*')), emit_infix("*")),
          expop(divide ⦂ number → number → number, infix(chrs('/')), emit_infix("/")),
          expop(expo ⦂ number → number → number, infix(chrs('^')), emit_infix("^")),
          expop(implies ⦂ bool → a → bool, [[]>>lexeme(word(if)),
                                            subexpr,
                                            []>>word(then),
                                            subexpr
                                           ],
                [_,[A,B],T]>>fmt_str(T, '~w => ~w', [A, B])),
          expop(implies ⦂ bool → a → bool, infix(chrs('=>')),
                [_,[A,B],T]>>fmt_str(T, '~w => ~w', [A, B])),
          expop(occurred ⦂ number → bool → bool,
                [[]>>lexeme(word(occurred)), chrs('('), subexpr,
                 chrs(','),subexpr,chrs(')')],
                [F,[A,B],T]>>fmt_str(T, '~w(~w, ~w)', [F, A, B])),
          expop(persisted ⦂ number → bool → bool,
                [[]>>lexeme(word(persisted)), chrs('('), subexpr,
                 chrs(','),subexpr,chrs(')')],
                [F,[A,B],T]>>fmt_str(T, '~w(~w, ~w)', [F, A, B]))
          %% expop(fncall2 ⦂ a → b → c,
          %%       [[]>>lexeme(word(FName)),
          %%        chrs('('),
          %%        subexpr,
          %%        chrs(','),
          %%        subexpr,
          %%        chrs(')')],
          %%       [_,[A,B],T]>>fmt_str(T, '??(~w, ~w)', [A, B]))
        ]}).


bool_expr(Env, V, FinalEnv) -->
    { fretish_expr_langdef(LangDef),
      get_dict(language, LangDef, Language)
    },
    expr(Language, Env, bool, V, FinalEnv).


% KWQ: ~ XOR -> => <-> <=> "IF be THEN be" "AT THE (PREVIOUS|NEXT) OCCURRENCE OF be, be"

% ----------------------------------------------------------------------

opt_comma --> [(_,',')].
opt_comma --> [].

any(N, S, P) --> any_(N, L, P), {string_codes(S, L)}.
any_(N, [C|CS], P) --> [(P,C)], {succ(M, N)}, any_(M, CS, _).
any_(0, [], span(99999,99999)) --> [].
any_(_, [], span(99998,99998)) --> [].

lexeme(R, P) --> ws(_), { ! }, lexeme(R, P).
lexeme(R, P) --> call(R, P).

lexeme(R, O, U) --> ws(_), { ! }, lexeme(R, O, U).
lexeme(R, O, U) --> call(R, O, U).

lexeme(R, O, P, U) --> ws(_), { ! }, lexeme(R, O, P, U).
lexeme(R, O, P, U) --> call(R, O, P, U).

lexeme(R, O, P, Q, U) --> ws(_), { ! }, lexeme(R, O, P, Q, U).
lexeme(R, O, P, Q, U) --> call(R, O, P, Q, U).

ws(span(N,N)) --> [(N,C)], { char_type(C, space) }.

prolog:message(bad_scope_mode(V, S)) -->
    [ 'Expected scope mode at ~w: ~w' - [S, V]].
