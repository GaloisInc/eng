% Parses the FRETish statements supported by the NASA Fret tool
% (https://github.com/NASA-SW-VnV/fret).

:- module(frettish, [ parse_fret/3, emit_fretish/2, emit_fretish/3 ]).

:- use_module('../englib').

%% Parses a FRETish English requirement to a Fret structured requirement, using
%% the definitions and templates provided to enhance the structured requirement.
%
%  Returns: fretment(scope_info({scope:{type:},[SCOPE_VAR_NAMES]),
%                    condition_info({condition:,
%                                    conditionTextRange:,
%                                    pre_condition:,
%                                    qualifier_word:,
%                                    regular_condition:},[COND_VAR_NAMES]),
%                    component_info({component:,
%                                    componentTextRange:,
%                                    component_name:},
%                    timing_info({timing:,
%                                 timingTextRange:},[TIMING_VAR_NAMES]),
%                    response_info({response:,
%                                   responseTextRange:,
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

fretish_parts(Fretment, ScopeText, CondText, CompText, TimingText, ResponseText) :-
    Fretment = fretment(scope_info(Scope, _ScopeVars),
                        condition_info(Condition, _CondVars),
                        component_info(Comp),
                        timing_info(Timing, _TimingVars),
                        response_info(Response, _RespVars)),
    scope_fretish(Scope, ScopeText),
    condition_fretish(Condition, CondText),
    component_fretish(Comp, CompText),
    timing_fretish(Timing, TimingText),
    response_fretish(Response, ResponseText).

scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, ST),
    member(ST, ["in", "after", "before"]),
    !,
    get_dict(scope_mode, Scope, M),
    format_str(ScopeText, '~w ~w ', [ST, M]).
scope_fretish(Scope, ScopeText) :-
    get_dict(scope, Scope, SC),
    get_dict(type, SC, "notin"),
    !,
    get_dict(scope_mode, Scope, M),
    format_str(ScopeText, 'unless in ~w ', [M]).
scope_fretish(_, "").

condition_fretish(Condition, "") :- get_dict(condition, Condition, "null"), !.
condition_fretish(Condition, CondText) :-
    get_dict(qualifier_word, Condition, QW),
    get_dict(regular_condition, Condition, RC),
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
                                format_str(Text, 'before ~w', [C]).
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "for"), !,
                                get_dict(duration, Timing, D),
                                (D == 1 -> U = tick ; U = ticks),
                                format_str(Text, 'for ~w ~w', [D, U]).
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "until"), !,
                                get_dict(stop_condition, Timing, C),
                                format_str(Text, 'until ~w', [C]).
timing_fretish(Timing, Text) :- get_dict(timing, Timing, "within"), !,
                                get_dict(duration, Timing, D),
                                (D == 1 -> U = tick ; U = ticks),
                                format_str(Text, 'within ~w ~w', [D, U]).
%% timing_fretish(Timing, "") :- get_dict(timing, Timing, "always"), !.
timing_fretish(Timing, Text) :- get_dict(timing, Timing, Text).

response_fretish(Response, ResponseText) :-
    get_dict(post_condition, Response, ResponseText).


%% ----------------------------------------------------------------------
%% Parsing

% scope conditions component shall timing responses
fretish(fretment(scope_info(Scope, ScopeVars),
                 condition_info(Condition, CondVars),
                 component_info(Comp),
                 timing_info(Timing, TimingVars),
                 response_info(Responses, RespVars)
                )) -->
    scope(Scope, ScopeVars),
    %% {format('....scope: ~w~n', [Scope])},
    conditions(Condition, CondVars),
    %% {format('....conditions: ~w with ~w~n', [Condition, CondVars])},
    component(Comp),
    %% {format('....component: ~w~n', [Comp])},
    lexeme(shall, _),
    !,
    timing(Timing, TimingVars),
    %% {format('....timing: ~w~n', [Timing])},
    lexeme(tok(satisfy)),
    !,
    ( responses(Responses, RespVars)
    ; word(EW, EP), { print_message(error, bad_response_text(EW, EP)) }
    ).

% scope conditions shall component timing responses
fretish(fretment(scope_info(Scope, ScopeVars),
                 condition_info(Condition, CondVars),
                 component_info(Comp),
                 timing_info(Timing, TimingVars),
                 response_info(Responses, RespVars)
                )) -->
    scope(Scope, ScopeVars),
    %% {format('....scope: ~w~n', [Scope])},
    conditions(Condition, CondVars),
    %% {format('....conditions: ~w with ~w~n', [Condition, CondVars])},
    lexeme(shall, _),
    !,
    component(Comp),
    %% {format('._..component: ~w~n', [Comp])},
    timing(Timing, TimingVars),
    %% {format('....timing: ~w~n', [Timing])},
    lexeme(tok(satisfy)),
    !,
    ( responses(Responses, RespVars)
    ; word(EW, EP), { print_message(error, bad_response_text(EW, EP)) }
    ).


prolog:message(bad_response_text(EW, EP)) -->
    [ 'Invalid FRETish Response specification at character ~w: ~w~n'
      - [ EP, EW ] ].

% --------------------------------------------------

scope(Scope, Vars) -->
    scope_(Scope, Vars), opt_comma(_).
scope(_{scope:_{type: null}}, []) --> [].

scope_(_{scope:_{type: "after"}, scope_mode:Mode,
         exclusive: false, required: false}, Vars) -->
    tok(after), scope_mode(allow_expr, Mode, Vars).
scope_(_{scope:_{type: "before"}, scope_mode:Mode,
         exclusive: false, required: false}, Vars) -->
    tok(before), scope_mode(allow_expr, Mode, Vars).
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars) -->
    tok(during), scope_mode(allow_expr, Mode, Vars).
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars) -->
    tok(while), scope_mode(allow_expr, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(if), lexeme(tok(not)), lexeme(tok(in)), scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars) -->
    tok(in), scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(when), lexeme(tok(not)), lexeme(tok(in)), scope_mode(mode_only, Mode, Vars).
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars) -->
    tok(unless), lexeme(tok(in)), scope_mode(mode_only, Mode, Vars).

% scope_mode: mode WORD | WORD mode | WORD   % KWQ: make vars for word (state = val)?
scope_mode(_, Mode, [Mode]) --> lexeme(tok(mode)), lexeme_(word, Mode).
scope_mode(_, Mode, [Mode]) --> lexeme_(word, Mode), lexeme(tok(mode)).
scope_mode(allow_expr, E, V) --> bool_expr(E, V).
scope_mode(_, Mode, [Mode]) --> lexeme_(word, Mode).

% --------------------------------------------------

conditions(ReqCond, Vars) -->
    tok(and), cond_(C,AllVars),
    { set_cond(C, AllVars, ReqCond, Vars) }.
conditions(ReqCond, Vars) -->
    cond_(C,AllVars),
    { set_cond(C, AllVars, ReqCond, Vars) }.
conditions(_{condition:"null"}, []) --> [].
set_cond(C, AllVars, ReqCond, Vars) :-
    list_to_set(AllVars, Vars),
    (get_dict(qualifier_word, C, "whenever")
    -> CND = "noTrigger"
    ; CND = "regular"
    ),
    put_dict(C, _{condition:CND}, ReqCond).


cond_(C,V) -->
    qcond1_(C0,V0), opt_comma(_), qcond2_(C0,V0,C,V), opt_comma(_).
cond_(C,V) --> qcond1_(C,V), opt_comma(_).

qcond1_(C,Vars) -->
    lexeme(tok(unless)),
    lexeme_(precond, E, Vars),
    { !, qcond1_false_("unless",E,C)}.
qcond1_(C,Vars) -->
    lexeme_(qualifier, Q),
    lexeme_(precond, E, Vars), lexeme(is, _), lexeme(tok(true)),
    { !, qcond1_true_(Q,E,C)}.
qcond1_(C,Vars) -->
    lexeme_(qualifier, Q),
    lexeme_(precond, E, Vars), lexeme(is, _), lexeme(false),
    { !, qcond1_false_(Q,E,C)}.
qcond1_(C,Vars) -->
    lexeme_(qualifier, Q), lexeme_(precond, E, Vars), { qcond1_true_(Q,E,C)}.
qcond1_true_(Q,E,C) :-
    format(atom(PCA), "(~w)", [E]), atom_string(PCA, PC),
    C = _{ qualifier_word:Q,
           pre_condition: PC,
           regular_condition: PC
         }.
qcond1_false_(Q,E,C) :-
    format(atom(PCA), "(!(~w))", [E]), atom_string(PCA, PC), % n.b. negated
    C = _{ qualifier_word:Q,
           pre_condition: PC,
           regular_condition: PC
         }.

qcond2_(C0,V0,C,Vars) --> tok(and), qcond2_and_(C0,V0,C,Vars).
qcond2_(C0,V0,C,Vars) -->
    or(_),
    qcond1_(C1,V1),
    { get_dict(pre_condition, C0, C0P),
      get_dict(pre_condition, C1, C1P),
      format(atom(PCA), '((~w) | (~w))', [ C0P, C1P ]), atom_string(PCA, PC),
      get_dict(qualifier_word, C1, C1QW),
      append(V0, V1, Vars),
      C = _{ qualifier_word: C1QW,  % XXX: always just uses *last* qualifier?!
             pre_condition: PC,
             regular_condition: PC
           }
    }.
qcond2_(C0,V0,C,Vars) --> qcond2_and_(C0,V0,C,Vars).
qcond2_and_(C0,V0,C,Vars) -->
    qcond1_(C1,V1),
    { get_dict(pre_condition, C0, C0P),
      get_dict(pre_condition, C1, C1P),
      format(atom(PCA), '((~w) & (~w))', [ C0P, C1P ]), atom_string(PCA, PC),
      get_dict(qualifier_word, C1, C1QW),
      append(V0, V1, Vars),
      C = _{ qualifier_word: C1QW,  % XXX: always just uses *last* qualifier?!
             pre_condition: PC,
             regular_condition: PC
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

component(_{component: Comp, component_name: Comp}) --> tok(the), lexeme_(word, Comp).
component(_{component: Comp, component_name: Comp}) --> word(Comp).

% --------------------------------------------------

timing(_{ timing: "always"}, []) --> lexeme(tok(always)).
timing(_{ timing: "after", duration: Duration }, []) --> lexeme(tok(after)),
                                                         duration_lower(Duration).
timing(_{ timing: "before", stop_condition: Cond }, Vars) --> lexeme(tok(before)),
                                                              bool_expr(Cond, Vars).
timing(_{ timing: "immediately" }, []) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(first, _), lexeme(tok(timepoint)).
timing(_{ timing: "finally" }, []) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(last, _), lexeme(tok(timepoint)).
timing(_{ timing: "next" }, []) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(next, _), lexeme(tok(timepoint)).
timing(_{ timing: "immediately" }, []) -->
    lexeme(tok(at)), lexeme(tok(the)), lexeme(same, _), lexeme(tok(timepoint)).
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
timing(_{ timing: "always"}) --> [],
                                 { writeln('warning, defaulting to always timing: may not have understood timing phrase') }.

duration_lower(D) --> duration_upper(D).
duration_upper(D) --> lexeme_(number, Dur),
                      lexeme(timeunit),
                      { % ensure JSON outputs numbers as a string because
                        % that's how FRET does it.
                        format_str(D, "~w ", [Dur])
                      }.

timeunit --> lexeme(tok(tick)).
timeunit --> lexeme(tok(ticks)).
timeunit --> lexeme(tok(hour)).
timeunit --> lexeme(tok(hours)).
timeunit --> lexeme(tok(minute)).
timeunit --> lexeme(tok(minutes)).
timeunit --> lexeme(tok(second)).
timeunit --> lexeme(tok(seconds)).
timeunit --> lexeme(tok(millisecond)).
timeunit --> lexeme(tok(milliseconds)).
timeunit --> lexeme(tok(microsecond)).
timeunit --> lexeme(tok(microseconds)).

% --------------------------------------------------

responses(_{response: "satisfaction", post_condition: E}, Vars) -->
    postcond(EP, AllVars),
    { format(atom(EA), '(~w)', [EP]), atom_string(EA, E),
      list_to_set(AllVars, Vars)
    }.

postcond(E, V) --> bool_expr(E, V).

% --------------------------------------------------

bool_expr(V, Vars) --> numeric_expr(LT, LV, LP),
                       relational_op(Op),
                       numeric_expr(RT, RV, RP),
                       { binary_(Op, LT, LV, LP, RT, RV, RP, XT, XV, XP) },
                       bool_exprMore(XT, XV, XP, V, Vars).
bool_expr(V, Vars) --> bool_term(LT, LV, LP),
                       bool_exprMore(LT, LV, LP, V, Vars).
bool_term("true", [], P) --> lexeme(token("true", P)).
bool_term("false", [], P) --> lexeme(token("false", P)).
bool_term(V, Vars, P) --> lexeme(token("if", IP)), bool_expr(Cnd, CVars),
                          lexeme(tok(then)), bool_expr(Thn, TVars),
                          { format_str(V, '(~w) => (~w)', [ Cnd, Thn ]),
                            append(CVars, TVars, Vars),
                            pos(IP, span(0,0), P)
                          }.
bool_term(V, [], P) --> lexeme(num, V, P).
bool_term(V, Vars, P) --> lexeme(not_, LP),
                          bool_term(NV, Vars, NP),
                          { format_str(V, '(! ~w)', [NV]) },
                          { pos(LP, NP, P) }.
bool_term(V, Vars, P) --> lexeme(lparen, LP),
                          bool_expr(PV, Vars),
                          { format_str(V, '(~w)', [PV]) },
                          lexeme(rparen, RP),
                          { pos(LP, RP, P) }.
bool_term(V, VS, P) --> lexeme(word, I, SP),
                        lexeme(lparen, _), args(AV, VS), lexeme(rparen, RP),
                        {
                            format_str(V, '~w(~w)', [I, AV]),
                            pos(SP, RP, P)
                        }.
bool_term(V, [V], P) --> lexeme(word, V, P).

% KWQ: ~ XOR -> => <-> <=> "IF be THEN be" "AT THE (PREVIOUS|NEXT) OCCURRENCE OF be, be"
bool_exprMore(LT, LV, LP, V, Vars) -->
    bool_exprMoreBin(LT, LV, LP, and_, "&", V, Vars).
bool_exprMore(LT, LV, LP, V, Vars) -->
    bool_exprMoreBin(LT, LV, LP, or_, "|", V, Vars).
bool_exprMore(LT, LV, _, LT, LV) --> [].

bool_exprMoreBin(LT, LV, LP, Matcher, Op, V, Vars) -->
    lexeme(Matcher), bool_term(RT, RV, RP),
    { binary_(Op, LT, LV, LP, RT, RV, RP, XS, XV, XP) },
    bool_exprMore(XS, XV, XP, V, Vars).
%% bool_exprMoreBin(LT, LV, LP, Matcher, Op, V, Vars, P) -->
%%     lexeme(Matcher, _), numeric_expr..., relational_op..., numeric_expr..., % if not requiring parens for these
%%     { binary_(Op, LT, LV, LP, RT, RV, RP, XS, XV, XP) },
%%     bool_exprMore(XS, XV, XP, V, Vars).

args(A, AV) --> arg(FA, FAV), lexeme(comma, _), args(MA, MAV),
                { format_str(A, "~w, ~w", [FA, MA]),
                  append(FAV, MAV, AV)
                }.
args(A, AV) --> arg(A, AV).
args("", []) --> [].

arg(A, AV) --> bool_expr(A, AV).
arg(A, AV) --> numeric_expr(A, AV, _).

numeric_expr(E, Vars, P) --> numeric_term(LT, LV, LP),
                             numeric_exprMore(LT, LV, LP, E, Vars, P).
numeric_term(V, [], P) --> lexeme(num, V, P).
numeric_term(V, [V], P) --> lexeme(word, V, P).
numeric_term(V, Vars, P) --> lexeme(minus_, LP),
                             numeric_term(NV, Vars, NP),
                             { format_str(V, '(-~w)', [NV]) },
                             { pos(LP, NP, P) }.
numeric_term(V, Vars, P) --> lexeme(lparen, LP),
                             numeric_expr(PV, Vars, _),
                             { format_str(V, '(~w)', [PV]) },
                             lexeme(rparen, RP),
                             { pos(LP, RP, P) }.
% KWQ: ^ - * / + - (E)
numeric_exprMore(LT, LV, LP, V, Vars, P) -->
    lexeme_(relational_op, RO),
    numeric_term(RT, RV, RP),
    { binary_(RO, LT, LV, LP, RT, RV, RP, XT, XV, XP) },
    numeric_exprMore(XT, XV, XP, V, Vars, P).
numeric_exprMore(LT, LV, LP, LT, LV, LP) --> [].

relational_op("!=") --> lexeme(neq_).
relational_op("<=") --> lexeme(lteq_).
relational_op(">=") --> lexeme(gteq_).
relational_op("=") --> lexeme(eq_).
relational_op("<") --> lexeme(lt_).
relational_op(">") --> lexeme(gt_).

num([N|NS], P) --> dig(N, PD), num(NS, PN), { pos(PD, PN, P) }.
num(N, P) --> dig(N, P).
dig(D, span(P, P)) --> [ (P,D) ], { char_type(D, digit) }.

binary_(Op, LT, LV, LP, RT, RV, RP, XS, XV, XP) :-
    format_str(XS, '~w ~w ~w', [ LT, Op, RT ]),
    pos(LP, RP, XP),
    append(LV, RV, XV).

% --------------------------------------------------

range(span(S,E), [S,E]).

opt_comma(P) --> [(N,',')], ws(PE), { pos(N, PE, P) }.
opt_comma(P) --> ws(P).

first(P) --> token("first", P).
is(P) --> token("is", P).
last(P) --> token("last", P).
next(P) --> token("next", P).
occurrence(P) --> token("occurrence", P).
of(P) --> token("of", P).
or(P) --> token("or", P).
same(P) --> token("same", P).
shall(P) --> token("shall", P).

lparen(span(P,P)) --> [(P,'(')].
rparen(span(P,P)) --> [(P,')')].
comma(span(P,P)) --> [(P,',')].
gteq_ --> [(P,'>'), (_, '=')].
lteq_ --> [(P,'<'), (_, '=')].
gt_ --> [(P,'>')].
lt_ --> [(P,'<')].
and_ --> [(P,'&')].
or_ --> [(P,'|')].
not_(span(P,P)) --> [(P,'!')].
eq_ --> [(P,'=')].
neq_ --> [(P,'!=')].
minus_(span(P,P)) --> [(P,'-')].

tok(M) --> word(W), { any_case_match([A], W), atom_string(A,M) }.
token(M,P) --> word(W,P), { any_case_match([M], W) }.

any_case_match(Candidates, Word) :- to_lower(Word, LCWord),
                                    member(LCWord, Candidates).

to_lower(I, O) :- atom_string(IA, I), downcase_atom(IA, OA), atom_string(OA, O).

word(W,P) --> [(N,C)], { word_char(C) }, wc(CS,PE),
              { string_codes(W, [C|CS]), pos(N, PE, P) }.
wc([C|CS],P) --> [(N,C)], { word_char(C) }, !, wc(CS,LP), { pos(N, LP, P) }.
wc([],span(0,0)) --> [].

word(W) --> [(N,C)], { word_char(C) }, wc(CS), { string_codes(W, [C|CS]) }.
wc([C|CS]) --> [(N,C)], { word_char(C) }, !, wc(CS).
wc([]) --> [].

word_char(C) :- \+ char_type(C, space),
                % Exclude things that might be individual tokens needing to be
                % recognized elsewhere in the grammar.
                \+ member(C, ['(', ')', '.', '!', '?', ':', ',',
                              %% '{', '}', '^', '[', ']', %% XXX?
                              '$',
                              '/']).

number([N|NS]) --> digit(N), number(NS).
number(N) --> digit(N).
digit(D) --> [ (_,D) ],
             { member(D, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) }. % KWQ: char_code numeric

lexeme(R) --> ws(_), !, lexeme(R).
lexeme(R) --> call(R).

lexeme(R, P) --> ws(_), { ! }, lexeme(R, P).
lexeme(R, P) --> call(R, P).

lexeme(R, O, P) --> ws(_), { ! }, lexeme(R, O, P).
lexeme(R, O, P) --> call(R, O, P).
lexeme_(R, O) --> ws(_), { ! }, lexeme_(R, O).
lexeme_(R, O) --> call(R, O).

lexeme(R, O, U, P) --> ws(_), { ! }, lexeme(R, O, U, P).
lexeme(R, O, U, P) --> call(R, O, U, P).
lexeme_(R, O, U) --> ws(_), { ! }, lexeme_(R, O, U).
lexeme_(R, O, U) --> call(R, O, U).

ws(span(N,N)) --> [(N,C)], { char_type(C, space) }.

pos(span(S,E), span(0,0), span(S,E)) :- !.
pos(span(S,_), span(_,E), span(S,E)) :- !.
pos(S, span(0,0), span(S,S)) :- !.
pos(S, span(_,E), span(S,E)).
