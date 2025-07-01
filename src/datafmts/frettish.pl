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
%                                    regular_condition:},[COND_VAR_NAMES]),
%                    component_info({component:}),
%                    timing_info({timing:[,duration:|stop_condition:]},
%                                [TIMING_VAR_NAMES]),
%                    response_info({response:,
%                                   post_condition:},[RESPONSE_VAR_NAMES]),
%
parse_fret(Context, English, FretMent) :-
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
                        condition_info(Condition, _),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Response, _)),
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
    expr_fretish(E, T),
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
    expr_fretish(E, T),
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
    expr_fretish(E, T),
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
    expr_fretish(E, T),
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
    get_dict(qualifier_word, Condition, QW),
    get_dict(regular_condition, Condition, CE),
    expr_fretish(CE, RC),
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
                                expr_fretish(C, CF),
                                format_str(Text, 'before ~w', [CF]).
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "for"), !,
                                get_dict(duration, Timing, D),
                                (D == 1 -> U = tick ; U = ticks),
                                format_str(Text, 'for ~w ~w', [D, U]).
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "until"), !,
                                get_dict(stop_condition, Timing, C),
                                expr_fretish(C, CF),
                                format_str(Text, 'until ~w', [CF]).
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "within"), !,
                                get_dict(duration, Timing, D),
                                (D == 1 -> U = tick ; U = ticks),
                                format_str(Text, 'within ~w ~w', [D, U]).
%% timing_fretish(Timing, "") :- get_dict(timing, Timing, "always"), !.
timing_fretish(Timing, Text) :- get_dict(timing, Timing, Text).

response_fretish(Response, ResponseText) :-
    get_dict(post_condition, Response, RE),
    expr_fretish(RE, ResponseText).


expr_fretish(implies(A,B), FRETish) :- expr_fretish_binop("=>", A, B, FRETish).
expr_fretish(and(A,B), FRETish) :- expr_fretish_binop("&", A, B, FRETish).
expr_fretish(or(A,B), FRETish) :- expr_fretish_binop("|", A, B, FRETish).
expr_fretish(eq(A,B), FRETish) :- expr_fretish_binop("=", A, B, FRETish).
expr_fretish(neq(A,B), FRETish) :- expr_fretish_binop("!=", A, B, FRETish).
expr_fretish(gt(A,B), FRETish) :- expr_fretish_binop(">", A, B, FRETish).
expr_fretish(lt(A,B), FRETish) :- expr_fretish_binop("<", A, B, FRETish).
expr_fretish(gteq(A,B), FRETish) :- expr_fretish_binop(">=", A, B, FRETish).
expr_fretish(lteq(A,B), FRETish) :- expr_fretish_binop("<=", A, B, FRETish).
expr_fretish(expo(A,B), FRETish) :- expr_fretish_binop("^", A, B, FRETish).
expr_fretish(mul(A,B), FRETish) :- expr_fretish_binop("*", A, B, FRETish).
expr_fretish(divide(A,B), FRETish) :- expr_fretish_binop("/", A, B, FRETish).
expr_fretish(mod(A,B), FRETish) :- expr_fretish_binop("mod", A, B, FRETish).
expr_fretish(add(A,B), FRETish) :- expr_fretish_binop("+", A, B, FRETish).
expr_fretish(sub(A,B), FRETish) :- expr_fretish_binop("-", A, B, FRETish).
expr_fretish(not(A), FRETish) :- expr_fretish(A, AF), format_str(FRETish, '(! ~w)', [AF]).
expr_fretish(call(F, Args), FRETish) :- maplist(expr_fretish, Args, AFS),
                                        intercalate(AFS, ", ", AStr),
                                        format_str(FRETish, '~w(~w)', [F, AStr]).
expr_fretish(ident(A), A).
expr_fretish(num(A), FRETish) :- atom_string(A, FRETish).
expr_fretish(BoolExprAST, FRETish) :-
    FRETish = BoolExprAST.

expr_fretish_binop(Op, A, B, FRETish) :-
    expr_fretish(A, AF),
    expr_fretish(B, BF),
    format_str(FRETish, "(~w ~w ~w)", [AF, Op, BF]).

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
fretment_vars(condition, fretment(_, condition_info(_{condition:"null"}, _), _, _, _), []) :- !.
fretment_vars(condition, fretment(_, condition_info(C, _), _, _, _), VS) :-
    get_dict(regular_condition, C, Expr),
    fretish_expr_langdef(LangDef),
    get_dict(language, LangDef, Language),
    extract_vars(Language, Expr, VS).
fretment_vars(timing, fretment(_, _, _, timing_info(_, vars(VS)), _), VS).
fretment_vars(response, fretment(_, _, _, _, response_info(R, _)), VS) :-
    get_dict(post_condition, R, Expr),
    fretish_expr_langdef(LangDef),
    get_dict(language, LangDef, Language),
    extract_vars(Language, Expr, VS).


% scope conditions component shall timing responses
fretish(fretment(scope_info(Scope, vars(ScopeVars)),
                 condition_info(Condition, vars(CondVars)),
                 component_info(Comp),
                 timing_info(Timing, vars(TimingVars)),
                 response_info(Responses, vars(RespVars))
                )) -->
    intro(Scope, ScopeVars, Condition, CondVars),
    target(Comp),
    !,
    timing(Timing, TimingVars),
    %% {format('....timing: ~w~n', [Timing])},
    lexeme(tok(satisfy)),
    !,
    ( responses(Responses, RespVars)
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

intro(Scope, ScopeVars, Condition, CondVars) -->
    scope(Scope, ScopeVars),
    %% {format('....scope: ~w~n', [Scope])},
    conditions(Condition, CondVars).
    %% {format('....conditions: ~w with ~w~n', [Condition, CondVars])}.

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

scope(Scope, Vars) --> scope_(Scope, Vars), opt_comma, !.
scope(_{scope:_{type: null}}, []) --> [].

scope_(_{scope:_{type: "after", exclusive: false, required: false},
         scope_mode:Mode}, Vars) -->
    tok(after),
    scope_mode(allow_expr, Mode, Vars).
scope_(_{scope:_{type: "onlyAfter",exclusive: false, required: false},
         scope_mode:Mode}, Vars) -->
    tok(only), lexeme(tok(after)),
    scope_mode(allow_expr, Mode, Vars).
scope_(_{scope:_{type: "before",exclusive: false, required: false},
         scope_mode:Mode}, Vars) -->
    tok(before),
    scope_mode(allow_expr, Mode, Vars).
scope_(_{scope:_{type: "onlyBefore",exclusive: false, required: false},
         scope_mode:Mode}, Vars) -->
    tok(only), lexeme(tok(before)),
    scope_mode(allow_expr, Mode, Vars).
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars) -->
    tok(during),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(except), lexeme(tok(during)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars) -->
    tok(in),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars) -->
    tok(when), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars) -->
    tok(if), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(except), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(except), lexeme(tok(if)), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(except), lexeme(tok(when)), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "onlyIn"}, scope_mode:Mode}, Vars) -->
    tok(only), lexeme(tok(during)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "onlyIn"}, scope_mode:Mode}, Vars) -->
    tok(only), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "onlyIn"}, scope_mode:Mode}, Vars) -->
    tok(only), lexeme(tok(if)), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "onlyIn"}, scope_mode:Mode}, Vars) -->
    tok(only), lexeme(tok(when)), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars) -->
    tok(while),
    scope_mode(allow_expr, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(except), lexeme(tok(while)),
    scope_mode(allow_expr, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(if), lexeme(tok(not)), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(when), lexeme(tok(not)), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(unless), lexeme(tok(in)),
    scope_mode(mode_only, Mode, Vars).

% scope_mode: mode WORD | WORD mode | WORD   % KWQ: make vars for word (state = val)?
scope_mode(_, mode(Mode), [Mode]) --> lexeme(tok(mode)), lexeme(word, Mode), !.
scope_mode(_, mode(Mode), [Mode]) --> lexeme(word, Mode), lexeme(tok(mode)), !.
scope_mode(allow_expr, E, V) --> bool_expr(E, V), !.  % n.b. this could also parse a "mode" as ident(_)
scope_mode(_, mode(Mode), [Mode]) --> lexeme(word, Mode).
scope_mode(_, bad, []) -->
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

conditions(fail, []) --> tok(help), [(_, '!')], !,
                         { print_message(help, condition_help), fail }.

conditions(ReqCond, Vars) --> tok(and), cond_(C,AllVars), !,
                              { set_cond(C, AllVars, ReqCond, Vars) }.
conditions(ReqCond, Vars) --> cond_(C,AllVars), !,
                              { set_cond(C, AllVars, ReqCond, Vars) }.
conditions(_{condition:"null"}, []) --> [].

set_cond(C, AllVars, ReqCond, Vars) :-
    list_to_set(AllVars, Vars),
    (get_dict(qualifier_word, C, "whenever") -> CND = "holding" ; CND = "regular"),
    put_dict(C, _{condition:CND}, ReqCond).


cond_(C,V) --> qcond1_(C0,V0), opt_comma, qcond2_(C0,V0,C,V), opt_comma.
cond_(C,V) --> qcond1_(C,V), opt_comma.

qcond1_(C,Vars) -->
    lexeme(tok(unless)),
    !,
    lexeme(precond, E, Vars),
    { !, qcond1_false_("unless",E,C)}.
qcond1_(C,Vars) -->
    lexeme(qualifier, Q),
    !,  % green cut
    qcond1__(C, Q, Vars).

qcond1__(C, Q, Vars) -->
    lexeme(precond, E, Vars),
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

qcond2_(C0,V0,C,Vars) --> tok(and), qcond2_and_(C0,V0,C,Vars).
qcond2_(C0,V0,C,Vars) -->
    tok(or),
    qcond1_(C1,V1),
    { get_dict(regular_condition, C0, C0P),
      get_dict(regular_condition, C1, C1P),
      get_dict(qualifier_word, C1, C1QW),
      append(V0, V1, Vars),
      C = _{ qualifier_word: C1QW,  % XXX: always just uses *last* qualifier?!
             pre_condition: or(C0P, C1P),
             regular_condition: or(C0P, C1P)
           }
    }.
qcond2_(C0,V0,C,Vars) --> qcond2_and_(C0,V0,C,Vars).
qcond2_and_(C0,V0,C,Vars) -->
    qcond1_(C1,V1),
    { get_dict(regular_condition, C0, C0P),
      get_dict(regular_condition, C1, C1P),
      format_str(PC, '((~w) & (~w))', [ C0P, C1P ]),
      get_dict(qualifier_word, C1, C1QW),
      append(V0, V1, Vars),
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

precond(E, V) --> bool_expr(E, V), !.  % green cut for performance

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

timing(fail, []) --> lexeme(tok(help)), [(_, '!')], !,
                     { print_message(help, timing_help), fail }.
timing(_{ timing: "always"}, []) --> lexeme(tok(always)).
timing(_{ timing: "after", duration: Duration }, []) --> lexeme(tok(after)),
                                                         duration_lower(Duration).
timing(_{ timing: "before", stop_condition: Cond }, Vars) --> lexeme(tok(before)),
                                                              bool_expr(Cond, Vars).
timing(_{ timing: "immediately" }, []) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(tok(first)), lexeme(tok(timepoint)).
timing(_{ timing: "finally" }, []) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(tok(last)), lexeme(tok(timepoint)).
timing(_{ timing: "next" }, []) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(tok(next)), lexeme(tok(timepoint)).
timing(_{ timing: "immediately" }, []) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(tok(same)), lexeme(tok(timepoint)).
timing(_{ timing: "for", duration: Duration }, []) --> lexeme(tok(for)),
                                                       duration_upper(Duration).
timing(_{ timing: "immediately" }, []) --> lexeme(tok(immediately)).
timing(_{ timing: "eventually" }, []) --> lexeme(tok(eventually)).
timing(_{ timing: "immediately" }, []) --> lexeme(tok(initially)).
timing(_{ timing: "finally" }, []) --> lexeme(tok(finally)).
timing(_{ timing: "never" }, []) --> lexeme(tok(never)).
timing(_{ timing: "until", stop_condition: Cond }, Vars) --> lexeme(tok(until)),
                                                             bool_expr(Cond, Vars).
timing(_{ timing: "within", duration: Duration }, []) --> lexeme(tok(within)),
                                                          duration_upper(Duration).
timing(fail, []) --> any(20, T, P),
                     { print_message(error, bad_timing(T, P)), !, fail }.

duration_lower(D) --> duration_upper(D).
duration_upper(D) --> lexeme(num, Dur),
                      lexeme(timeunit),
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

responses(fail, []) --> lexeme(tok(help)), [(_, '!')], !,
                        { print_message(help, responses_help), fail }.
responses(_{response: "satisfaction", post_condition: EP}, Vars) -->
    postcond(EP, AllVars),
    { list_to_set(AllVars, Vars)}.

postcond(E, V) --> bool_expr(E, V).

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

bool_expr(V, Vars) --> numeric_expr(LT, LV),
                       relational_op(Op),
                       !,
                       bool_expr_(V, Vars, Op, LT, LV).
bool_expr(V, Vars) --> bool_term(LT, LV),
                       !,
                       bool_exprMore(LT, LV, V, Vars).
bool_expr(V, []) --> any(20, V, P),
                     { print_message(error, invalid_bool_expr(V, P)), !, fail }.
bool_expr_(V, Vars, Op, LT, LV) -->
    numeric_expr(RT, RV),
    !,
    { binary_(Op, LT, LV, RT, RV, XT, XV) },
    bool_exprMore(XT, XV, V, Vars).
bool_expr_(bad, [], Op, LT, _) -->
    any(20, V, P),
    { print_message(error, invalid_partial_bool_expr(Op, LT, V, P)), !, fail }.

bool_term(true, []) --> lexeme(tok(true)).
bool_term(false, []) --> lexeme(tok(false)).
bool_term(V, Vars) --> lexeme(tok(if)),   bool_expr(Cnd, CVars),
                       lexeme(tok(then)), bool_expr(Thn, TVars),
                       { binary_(implies, Cnd, CVars, Thn, TVars, V, Vars) }.
bool_term(num(V), []) --> lexeme(num, V).
bool_term(not(V), Vars) --> lexeme(not_), bool_term(V, Vars).
bool_term(V, Vars) --> lexeme(lparen), bool_expr(V, Vars), lexeme(rparen).
bool_term(call(I, AV), VS) --> lexeme(word, I),  % function name
                               lexeme(lparen),
                               args(AV, VS), lexeme(rparen),
                               { atom_string(F, I), V =.. [F, AV] }.
bool_term(ident(V), [V]) --> lexeme(word, V).  % variable

% KWQ: ~ XOR -> => <-> <=> "IF be THEN be" "AT THE (PREVIOUS|NEXT) OCCURRENCE OF be, be"
bool_exprMore(LT, LV, V, Vars) -->
    bool_exprMoreBin(LT, LV, and_, and, V, Vars).
bool_exprMore(LT, LV, V, Vars) -->
    bool_exprMoreBin(LT, LV, or_, or, V, Vars).
bool_exprMore(LT, LV, LT, LV) --> [].

bool_exprMoreBin(LT, LV, Matcher, Op, V, Vars) -->
    lexeme(Matcher), bool_expr(RT, RV),
    { binary_(Op, LT, LV, RT, RV, XS, XV) },
    bool_exprMore(XS, XV, V, Vars).
%% bool_exprMoreBin(LT, LV, Matcher, Op, V, Vars, P) -->
%%     lexeme(Matcher, _), numeric_expr..., relational_op..., numeric_expr..., % if not requiring parens for these
%%     { binary_(Op, LT, LV, RT, RV, XS, XV) },
%%     bool_exprMore(XS, XV, V, Vars).

args([FA|MA], AV) --> arg(FA, FAV), lexeme(comma_), args(MA, MAV),
                { format_str(A, "~w, ~w", [FA, MA]),
                  append(FAV, MAV, AV)
                }.
args([A], AV) --> arg(A, AV).

arg(A, AV) --> lexeme(bool_expr(A, AV)).
arg(A, AV) --> lexeme(numeric_expr(A, AV)).

numeric_expr(E, Vars) --> numeric_term(LT, LV),
                          numeric_exprMore(LT, LV, E, Vars).
numeric_term(num(V), []) --> lexeme(num, V).
numeric_term(neg(V), Vars) --> lexeme(minus_), numeric_term(V, Vars).
numeric_term(ident(V), [V]) --> lexeme(word, V).  % variable
numeric_term(V, Vars) --> lexeme(lparen),
                          numeric_expr(V, Vars),
                          lexeme(rparen).

% KWQ: (E)
numeric_exprMore(LT, LV, V, Vars) -->
    lexeme(numeric_op, RO),
    numeric_expr(RT, RV),
    { binary_(RO, LT, LV, RT, RV, XT, XV) },
    numeric_exprMore(XT, XV, V, Vars).
numeric_exprMore(LT, LV, LT, LV) --> [].

numeric_op(expo) --> lexeme(exp_).
numeric_op(mul) --> lexeme(times_).
numeric_op(divide) --> lexeme(div_).
numeric_op(add) --> lexeme(plus_).
numeric_op(sub) --> lexeme(minus_).

relational_op(neq) --> lexeme(neq_).
relational_op(lteq) --> lexeme(lteq_).
relational_op(gteq) --> lexeme(gteq_).
relational_op(eq) --> lexeme(eq_).
relational_op(lt) --> lexeme(lt_).
relational_op(gt) --> lexeme(gt_).

num(V) --> num_(Digits), { to_num(Digits, 0, V) }.
num_([N|NS]) --> dig(N, _), num_(NS).
num_([N]) --> dig(N, _).

to_num([], A, A).
to_num([D|DS], A, V) :- N is A * 10 + D, to_num(DS, N, V).

dig(D, span(P, P)) --> [ (P,C) ], { char_type(C, digit),
                                    atom_codes(C, [CS]),
                                    atom_codes('0', [ZS]),
                                    plus(ZS, D, CS)
                                  }.

binary_(Op, LT, LV, RT, RV, XS, XV) :- XS =.. [Op, LT, RT], append(LV, RV, XV).

% --------------------------------------------------

opt_comma --> [(_,',')], ws(_).
opt_comma --> ws(_).

lparen --> [(_,'(')].
rparen --> [(_,')')].
comma_ --> [(_,',')].
gteq_ --> [(_,'>'), (_, '=')].
lteq_ --> [(_,'<'), (_, '=')].
gt_ --> [(_,'>')].
lt_ --> [(_,'<')].
and_ --> [(_,'&')].
or_ --> [(_,'|')].
neq_ --> [(_,'!'), (_, '=')].
not_ --> [(_,'!')].
eq_ --> [(_,'=')].
minus_ --> [(_,'-')].
plus_ --> [(_,'+')].
times_ --> [(_,'*')].
div_ --> [(_,'/')].
exp_ --> [(_,'^')].

% ----------------------------------------------------------------------

tok(M) --> word(W), { any_case_match([A], W), atom_string(A,M) }.
token(M,P) --> word(W,P), { any_case_match([M], W) }.

any_case_match(Candidates, Word) :- to_lower(Word, LCWord),
                                    member(LCWord, Candidates).

to_lower(I, O) :- atom_string(IA, I), downcase_atom(IA, OA), atom_string(OA, O).

word(W,P) --> [(N,C)], { word_char(C), \+ char_type(C, digit) },
              wc(CS,PE),
              { string_codes(W, [C|CS]), pos(N, PE, P) }.
wc([C|CS],P) --> [(N,C)], { word_char(C) }, !, wc(CS,LP), { pos(N, LP, P) }.
wc([],span(0,0)) --> [].

word(W) --> [(_N,C)], { word_char(C) }, wc(CS), { string_codes(W, [C|CS]) }.
wc([C|CS]) --> [(_N,C)], { word_char(C) }, !, wc(CS).
wc([]) --> [].

word_char(C) :- \+ char_type(C, space),
                % Exclude things that might be individual tokens needing to be
                % recognized elsewhere in the grammar.
                \+ member(C, ['(', ')', '.', '!', '?', ':', ',',
                              %% '{', '}', '^', '[', ']', %% XXX?
                              '$',
                              '=',
                              '/']).

any(N, S, P) --> any_(N, L, P), {string_codes(S, L)}.
any_(N, [C|CS], P) --> [(P,C)], {succ(M, N)}, any_(M, CS, _).
any_(0, [], span(99999,99999)) --> [].
any_(_, [], span(99998,99998)) --> [].

lexeme(R) --> ws(_), !, lexeme(R).
lexeme(R) --> call(R).

lexeme(R, P) --> ws(_), { ! }, lexeme(R, P).
lexeme(R, P) --> call(R, P).

lexeme(R, O, U) --> ws(_), { ! }, lexeme(R, O, U).
lexeme(R, O, U) --> call(R, O, U).

ws(span(N,N)) --> [(N,C)], { char_type(C, space) }.

pos(span(S,E), span(0,0), span(S,E)) :- !.
pos(span(S,_), span(_,E), span(S,E)) :- !.
pos(S, span(0,0), span(S,S)) :- !.
pos(S, span(_,E), span(S,E)).

prolog:message(bad_scope_mode(V, S)) -->
    [ 'Expected scope mode at ~w: ~w' - [S, V]].
prolog:message(invalid_bool_expr(V, S)) -->
    { bool_exprHelp(H) },
    [ 'Expected boolean expression at character ~w: ~w~w' - [S, V, H]].
prolog:message(invalid_partial_bool_expr(Op, LT, V, S)) -->
    { bool_exprHelp(H) },
    [ 'Incomplete boolean expression at character ~w: ~w~n  Preceeded by: ~w ~w~w'
      - [S, V, LT, Op, H]].
