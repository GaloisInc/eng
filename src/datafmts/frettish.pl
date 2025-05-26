% Parses the FRETtish statements supported by the NASA Fret tool
% (https://github.com/NASA-SW-VnV/fret).

:- module(frettish, [ parse_fret/3 ]).

:- use_module('../englib').

%% Parses a Frettish English requirement to a Fret structured requirement, using
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
    phrase(frettish(FretMent), Input, Remaining),
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
    format('Unexpected frettish extra not parsed @ ~w, offset ~w: "~w"~n',
           [ Context, P, Str ]).

unenumerate([], []).
unenumerate([C|OS], [(_,C)|CS]) :- unenumerate(OS, CS).

% scope conditions component shall timing responses
frettish(fretment(scope_info(Scope, ScopeVars),
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
    lexeme(satisfy, SP),
    !,
    ( responses(SP, Responses, RespVars)
    ; word(EW, EP), { print_message(error, bad_response_text(EW, EP)) }
    ).

% scope conditions shall component timing responses
frettish(fretment(scope_info(Scope, ScopeVars),
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
    lexeme(satisfy, SP),
    !,
    ( responses(SP, Responses, RespVars)
    ; word(EW, EP), { print_message(error, bad_response_text(EW, EP)) }
    ).


prolog:message(bad_response_text(EW, EP)) -->
    [ 'Invalid FRETTISH Response specification at character ~w: ~w~n'
      - [ EP, EW ] ].

% --------------------------------------------------

scope(Scope, Vars) -->
    scope_(SubScope, Vars, Pos), opt_comma(_),
    { range(Pos, Range),
      put_dict(SubScope, _{scopeTextRange:Range}, Scope)
    }.
scope(_{scope:_{type: null}}, []) --> [].

scope_(_{scope:_{type: "after"}, scope_mode:Mode,
         exclusive: false, required: false}, Vars, Pos) -->
    after(P0),
    scope_mode(allow_expr, Mode, Vars, PM), { pos(P0, PM, Pos) }.
scope_(_{scope:_{type: "before"}, scope_mode:Mode,
         exclusive: false, required: false}, Vars, Pos) -->
    before(P0),
    scope_mode(allow_expr, Mode, Vars, PM), { pos(P0, PM, Pos) }.
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars, Pos) -->
    during(P0),
    scope_mode(allow_expr, Mode, Vars, PM), { pos(P0, PM, Pos) }.
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars, Pos) -->
    while(P0),
    scope_mode(allow_expr, Mode, Vars, PM), { pos(P0, PM, Pos) }.
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars, Pos) -->
    if(P0), lexeme(not, _), lexeme(in, _),
    scope_mode(mode_only, Mode, Vars, PM),
    { pos(P0, PM, Pos) }.
scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars, Pos) -->
    in(P0),
    scope_mode(mode_only, Mode, Vars, PM), { pos(P0, PM, Pos) }.
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars, Pos) -->
    when(P0), lexeme(not, _), lexeme(in, _),
    scope_mode(mode_only, Mode, Vars, PM),
    { pos(P0, PM, Pos) }.
scope_(_{scope:_{type: "notin"}, scope_mode:Mode}, Vars, Pos) -->
    unless(P0), lexeme(in, _),
    scope_mode(mode_only, Mode, Vars, PM),
    { pos(P0, PM, Pos) }.

% scope_mode: mode WORD | WORD mode | WORD   % KWQ: make vars for word (state = val)?
scope_mode(_, Mode, [Mode], P) -->
    lexeme(mode, P0), lexeme(word, Mode, PE),
    { pos(P0, PE, P) }.
scope_mode(_, Mode, [Mode], P) -->
    lexeme(word, Mode, P0), lexeme(mode, PE),
    { pos(P0, PE, P) }.
scope_mode(allow_expr, E, V, P) --> bool_expr(E, V, P).
scope_mode(_, Mode, [Mode], P) --> lexeme(word, Mode, P).

% --------------------------------------------------

conditions(ReqCond, Vars) -->
    and(P0), cond_(C,CP,AllVars),
    { pos(P0, CP, P), set_cond(C, P, AllVars, ReqCond, Vars) }.
conditions(ReqCond, Vars) -->
    cond_(C,CP,AllVars),
    { set_cond(C, CP, AllVars, ReqCond, Vars) }.
conditions(_{condition:"null"}, []) --> [].
set_cond(C, CP, AllVars, ReqCond, Vars) :-
    range(CP, Range),
    list_to_set(AllVars, Vars),
    (get_dict(qualifier_word, C, "whenever")
    -> CND = "noTrigger"
    ; CND = "regular"
    ),
    put_dict(C, _{condition:CND, conditionTextRange:Range}, ReqCond).


cond_(C,CP,V) -->
    qcond1_(C0,CP0,V0), opt_comma(_), qcond2_(C0,CP0,V0,C,CP,V), opt_comma(_).
cond_(C,CP,V) --> qcond1_(C,CP,V), opt_comma(_).

qcond1_(C,P,Vars) -->
    lexeme(unless, QP),
    lexeme(precond, E, Vars, EP),
    { !, pos(QP, EP, P), qcond1_false_("unless",E,C)}.
qcond1_(C,P,Vars) -->
    lexeme(qualifier, Q, QP),
    lexeme(precond, E, Vars, _), lexeme(is, _), lexeme(true, EP),
    { !, pos(QP, EP, P), qcond1_true_(Q,E,C)}.
qcond1_(C,P,Vars) -->
    lexeme(qualifier, Q, QP),
    lexeme(precond, E, Vars, _), lexeme(is, _), lexeme(false, EP),
    { !, pos(QP, EP, P), qcond1_false_(Q,E,C)}.
qcond1_(C,P,Vars) -->
    lexeme(qualifier, Q, QP), lexeme(precond, E, Vars, CP),
    { pos(QP, CP, P), qcond1_true_(Q,E,C)}.
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

qcond2_(C0,P0,V0,C,P,Vars) --> and(_), qcond2_and_(C0,P0,V0,C,P,Vars).
qcond2_(C0,P0,V0,C,P,Vars) -->
    or(_),
    qcond1_(C1,P1,V1),
    { pos(P0, P1, P),
      get_dict(pre_condition, C0, C0P),
      get_dict(pre_condition, C1, C1P),
      format(atom(PCA), '((~w) | (~w))', [ C0P, C1P ]), atom_string(PCA, PC),
      get_dict(qualifier_word, C1, C1QW),
      append(V0, V1, Vars),
      C = _{ qualifier_word: C1QW,  % XXX: always just uses *last* qualifier?!
             pre_condition: PC,
             regular_condition: PC
           }
    }.
qcond2_(C0,P0,V0,C,P,Vars) --> qcond2_and_(C0,P0,V0,C,P,Vars).
qcond2_and_(C0,P0,V0,C,P,Vars) -->
    qcond1_(C1,P1,V1),
    { pos(P0, P1, P),
      get_dict(pre_condition, C0, C0P),
      get_dict(pre_condition, C1, C1P),
      format(atom(PCA), '((~w) & (~w))', [ C0P, C1P ]), atom_string(PCA, PC),
      get_dict(qualifier_word, C1, C1QW),
      append(V0, V1, Vars),
      C = _{ qualifier_word: C1QW,  % XXX: always just uses *last* qualifier?!
             pre_condition: PC,
             regular_condition: PC
           }
    }.

qualifier("upon", P) --> upon(P).
qualifier("whenever", P) --> whenever(P).
qualifier("when", P) --> when(P).
qualifier("unless", P) --> unless(P).
qualifier("where", P) --> where(P).
qualifier("if", P) --> if(P).

precond(E, V, P) --> bool_expr(E, V, P), !.  % green cut for performance

% --------------------------------------------------

component(_{ component: Comp,
             component_name: Comp,
             componentTextRange:Range }) -->
    the(TP), lexeme(word, Comp, CP), { pos(TP, CP, P), range(P, Range) }.
component(_{ component: Comp,
             component_name: Comp,
             componentTextRange:Range }) -->
    word(Comp, P), { range(P, Range) }.

% --------------------------------------------------

timing(_{ timing: "always", timingTextRange:Range}, []) -->
    lexeme(always, P), {range(P, Range)}.
timing(_{ timing: "after", duration: Duration, timingTextRange:Range}, []) -->
    lexeme(after, SP), duration_lower(Duration, LP),
    { pos(SP, LP, P), range(P, Range) }.
timing(_{ timing: "before", stop_condition: Cond, timingTextRange:Range}, Vars) -->
    lexeme(before, SP), bool_expr(Cond, Vars, LP),
    { pos(SP, LP, P), range(P, Range) }.
timing(_{ timing: "immediately", timingTextRange:Range}, []) -->
    lexeme(at, SP), lexeme(the, _), lexeme(first, _), lexeme(timepoint, TP),
    { pos(SP, TP, P), range(P, Range) }.
timing(_{ timing: "finally", timingTextRange:Range}, []) -->
    lexeme(at, SP), lexeme(the, _), lexeme(last, _), lexeme(timepoint, TP),
    { pos(SP, TP, P), range(P, Range) }.
timing(_{ timing: "next", timingTextRange:Range}, []) -->
    lexeme(at, SP), lexeme(the, _), lexeme(next, _), lexeme(timepoint, TP),
    { pos(SP, TP, P), range(P, Range) }.
timing(_{ timing: "immediately", timingTextRange:Range}, []) -->
    lexeme(at, SP), lexeme(the, _), lexeme(same, _), lexeme(timepoint, TP),
    { pos(SP, TP, P), range(P, Range) }.
timing(_{ timing: "for", duration: Duration, timingTextRange:Range}, []) -->
    lexeme(for, SP), duration_upper(Duration, LP),
    { pos(SP, LP, P), range(P, Range) }.
timing(_{ timing: "immediately", timingTextRange:Range}, []) -->
    lexeme(immediately, P),
    { range(P, Range) }.
timing(_{ timing: "eventually", timingTextRange:Range}, []) -->
    lexeme(eventually, P),
    { range(P, Range) }.
timing(_{ timing: "immediately", timingTextRange:Range}, []) -->
    lexeme(initially, P),
    { range(P, Range) }.
timing(_{ timing: "finally", timingTextRange:Range}, []) -->
    lexeme(finally, P),
    { range(P, Range) }.
timing(_{ timing: "never", timingTextRange:Range}, []) -->
    lexeme(never, P), {range(P, Range)}.
timing(_{ timing: "until", stop_condition: Cond, timingTextRange:Range}, Vars) -->
    lexeme(until, SP), bool_expr(Cond, Vars, LP),
    { pos(SP, LP, P), range(P, Range) }.
timing(_{ timing: "within", duration: Duration, timingTextRange:Range}, []) -->
    lexeme(within, SP), duration_upper(Duration, LP),
    { pos(SP, LP, P), range(P, Range) }.
timing(_{ timing: "always"}) --> [],
                                 { writeln('warning, defaulting to always timing: may not have understood timing phrase') }.

duration_lower(D, P) --> duration_upper(D, P).
duration_upper(D, P) --> lexeme(number, Dur, SP),
                          lexeme(timeunit, LP),
                         { pos(SP, LP, P),
                           % ensure JSON outputs numbers as a string because
                           % that's how FRET does it.
                           format(atom(DA), "~w ", [Dur]),
                           atom_string(DA, D)
                         }.

timeunit(P) --> lexeme(token, "tick", P).
timeunit(P) --> lexeme(token, "ticks", P).
timeunit(P) --> lexeme(token, "hour", P).
timeunit(P) --> lexeme(token, "hours", P).
timeunit(P) --> lexeme(token, "minute", P).
timeunit(P) --> lexeme(token, "minutes", P).
timeunit(P) --> lexeme(token, "second", P).
timeunit(P) --> lexeme(token, "seconds", P).
timeunit(P) --> lexeme(token, "millisecond", P).
timeunit(P) --> lexeme(token, "milliseconds", P).
timeunit(P) --> lexeme(token, "microsecond", P).
timeunit(P) --> lexeme(token, "microseconds", P).

% --------------------------------------------------

responses(SP, _{response: "satisfaction",
                post_condition: E,
                responseTextRange: Range
               }, Vars) -->
    postcond(EP, AllVars, CP),
    { format(atom(EA), '(~w)', [EP]), atom_string(EA, E),
      pos(SP, CP, P),
      list_to_set(AllVars, Vars),
      range(P, Range)
    }.

postcond(E, V, P) --> bool_expr(E, V, P).

% --------------------------------------------------

bool_expr(V, Vars, P) --> numeric_expr(LT, LV, LP),
                          relational_op(Op, _),
                          numeric_expr(RT, RV, RP),
                          { binary_(Op, LT, LV, LP, RT, RV, RP, XT, XV, XP) },
                          bool_exprMore(XT, XV, XP, V, Vars, P).
bool_expr(V, Vars, P) --> bool_term(LT, LV, LP),
                          bool_exprMore(LT, LV, LP, V, Vars, P).
bool_term("true", [], P) --> lexeme(true, P).
bool_term("false", [], P) --> lexeme(false, P).
bool_term(V, Vars, P) --> lexeme(if, IP), bool_expr(Cnd, CVars, _CP),
                          lexeme(then, _), bool_expr(Thn, TVars, TP),
                          { format(atom(X), '(~w) => (~w)', [ Cnd, Thn ]),
                            atom_string(X, V),
                            append(CVars, TVars, Vars),
                            pos(IP, TP, P)
                          }.
bool_term(V, [], P) --> lexeme(num, V, P).
bool_term(V, Vars, P) --> lexeme(not_, LP),
                          bool_term(NV, Vars, NP),
                          { format(atom(X), '(! ~w)', [NV]), atom_string(X, V) },
                          { pos(LP, NP, P) }.
bool_term(V, Vars, P) --> lexeme(lparen, LP),
                          bool_expr(PV, Vars, _),
                          { format(atom(X), '(~w)', [PV]), atom_string(X, V) },
                          lexeme(rparen, RP),
                          { pos(LP, RP, P) }.
bool_term(V, VS, P) --> lexeme(word, I, SP),
                        lexeme(lparen, _), args(AV, VS), lexeme(rparen, RP),
                        {
                            format(atom(X), '~w(~w)', [I, AV]),
                            atom_string(X, V),
                            pos(SP, RP, P)
                        }.
bool_term(V, [V], P) --> lexeme(word, V, P).

% KWQ: ~ XOR -> => <-> <=> "IF be THEN be" "AT THE (PREVIOUS|NEXT) OCCURRENCE OF be, be"
bool_exprMore(LT, LV, LP, V, Vars, P) -->
    bool_exprMoreBin(LT, LV, LP, and_, "&", V, Vars, P).
bool_exprMore(LT, LV, LP, V, Vars, P) -->
    bool_exprMoreBin(LT, LV, LP, or_, "|", V, Vars, P).
bool_exprMore(LT, LV, LP, LT, LV, LP) --> [].

bool_exprMoreBin(LT, LV, LP, Matcher, Op, V, Vars, P) -->
    lexeme(Matcher, _), bool_term(RT, RV, RP),
    { binary_(Op, LT, LV, LP, RT, RV, RP, XS, XV, XP) },
    bool_exprMore(XS, XV, XP, V, Vars, P).
%% bool_exprMoreBin(LT, LV, LP, Matcher, Op, V, Vars, P) -->
%%     lexeme(Matcher, _), numeric_expr..., relational_op..., numeric_expr..., % if not requiring parens for these
%%     { binary_(Op, LT, LV, LP, RT, RV, RP, XS, XV, XP) },
%%     bool_exprMore(XS, XV, XP, V, Vars, P).

args(A, AV) --> arg(FA, FAV), lexeme(comma, _), args(MA, MAV),
                { format(atom(X), "~w, ~w", [FA, MA]),
                  atom_string(X, A),
                  append(FAV, MAV, AV)
                }.
args(A, AV) --> arg(A, AV).
args("", []) --> [].

arg(A, AV) --> bool_expr(A, AV, _).
arg(A, AV) --> numeric_expr(A, AV, _).

numeric_expr(E, Vars, P) --> numeric_term(LT, LV, LP),
                             numeric_exprMore(LT, LV, LP, E, Vars, P).
numeric_term(V, [], P) --> lexeme(num, V, P).
numeric_term(V, [V], P) --> lexeme(word, V, P).
numeric_term(V, Vars, P) --> lexeme(minus_, LP),
                             numeric_term(NV, Vars, NP),
                             { format(atom(X), '(-~w)', [NV]), atom_string(X, V) },
                             { pos(LP, NP, P) }.
numeric_term(V, Vars, P) --> lexeme(lparen, LP),
                             numeric_expr(PV, Vars, _),
                             { format(atom(X), '(~w)', [PV]), atom_string(X, V) },
                             lexeme(rparen, RP),
                             { pos(LP, RP, P) }.
% KWQ: ^ - * / + - (E)
numeric_exprMore(LT, LV, LP, V, Vars, P) -->
    lexeme(relational_op, RO, _),
    numeric_term(RT, RV, RP),
    { binary_(RO, LT, LV, LP, RT, RV, RP, XT, XV, XP) },
    numeric_exprMore(XT, XV, XP, V, Vars, P).
numeric_exprMore(LT, LV, LP, LT, LV, LP) --> [].

relational_op("!=", Pos) --> lexeme(neq_, Pos).
relational_op("<=", Pos) --> lexeme(lteq_, Pos).
relational_op(">=", Pos) --> lexeme(gteq_, Pos).
relational_op("=", Pos) --> lexeme(eq_, Pos).
relational_op("<", Pos) --> lexeme(lt_, Pos).
relational_op(">", Pos) --> lexeme(gt_, Pos).

num([N|NS], P) --> dig(N, PD), num(NS, PN), { pos(PD, PN, P) }.
num(N, P) --> dig(N, P).
dig(D, span(P, P)) --> [ (P,D) ], { char_type(D, digit) }.

binary_(Op, LT, LV, LP, RT, RV, RP, XS, XV, XP) :-
    format(atom(X), '~w ~w ~w', [ LT, Op, RT ]),
    atom_string(X, XS),
    pos(LP, RP, XP),
    append(LV, RV, XV).

% --------------------------------------------------

range(span(S,E), [S,E]).

opt_comma(P) --> [(N,',')], ws(PE), { pos(N, PE, P) }.
opt_comma(P) --> ws(P).

after(P) --> token("after", P).
always(P) --> token("always", P).
and(P) --> token("and", P).
at(P) --> token("at", P).
before(P) --> token("before", P).
during(P) --> token("during", P).
eventually(P) --> token("eventually", P).
false(P) --> token("false", P).
finally(P) --> token("finally", P).
first(P) --> token("first", P).
for(P) --> token("for", P).
if(P) --> token("if", P).
immediately(P) --> token("immediately", P).
initially(P) --> token("initially", P).
in(P) --> token("in", P).
is(P) --> token("is", P).
last(P) --> token("last", P).
mode(P) --> token("mode", P).
never(P) --> token("never", P).
next(P) --> token("next", P).
not(P) --> token("not", P).
occurrence(P) --> token("occurrence", P).
of(P) --> token("of", P).
or(P) --> token("or", P).
same(P) --> token("same", P).
satisfy(P) --> token("satisfy", P).
shall(P) --> token("shall", P).
then(P) --> token("then", P).
the(P) --> token("the", P).
timepoint(P) --> token("timepoint", P).
true(P) --> token("true", P).
unless(P) --> token("unless", P).
until(P) --> token("until", P).
upon(P) --> token("upon", P).
whenever(P) --> token("whenever", P).
when(P) --> token("when", P).
where(P) --> token("where", P).
while(P) --> token("while", P).
within(P) --> token("within", P).

lparen(span(P,P)) --> [(P,'(')].
rparen(span(P,P)) --> [(P,')')].
comma(span(P,P)) --> [(P,',')].
gteq_(span(P,P)) --> [(P,'>'), (_, '=')].
lteq_(span(P,P)) --> [(P,'<'), (_, '=')].
gt_(span(P,P)) --> [(P,'>')].
lt_(span(P,P)) --> [(P,'<')].
and_(span(P,P)) --> [(P,'&')].
or_(span(P,P)) --> [(P,'|')].
not_(span(P,P)) --> [(P,'!')].
eq_(span(P,P)) --> [(P,'=')].
neq_(span(P,P)) --> [(P,'!=')].
minus_(span(P,P)) --> [(P,'-')].

token(M,P) --> word(W,P), { any_case_match([M], W) }.

any_case_match(Candidates, Word) :- to_lower(Word, LCWord),
                                    member(LCWord, Candidates).

to_lower(I, O) :- atom_string(IA, I), downcase_atom(IA, OA), atom_string(OA, O).

word(W,P) --> [(N,C)], { word_char(C) }, wc(CS,PE),
              { string_codes(W, [C|CS]), pos(N, PE, P) }.
wc([C|CS],P) --> [(N,C)], { word_char(C) }, !, wc(CS,LP), { pos(N, LP, P) }.
wc([],span(0,0)) --> [].

word_char(C) :- \+ char_type(C, space),
                % Exclude things that might be individual tokens needing to be
                % recognized elsewhere in the grammar.
                \+ member(C, ['(', ')', '.', '!', '?', ':', ',',
                              %% '{', '}', '^', '[', ']', %% XXX?
                              '$',
                              '/']).

number([N|NS], P) --> digit(N, SP), number(NS, NP), {pos(SP, NP, P)}.
number(N, P) --> digit(N, P).
digit(D, P) --> [ (P,D) ],
                { member(D, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) }. % KWQ: char_code numeric

lexeme(R) --> ws(_), { !, writeln(l0) }, lexeme(R).
lexeme(R) --> call(R), { writeln(l1), writeln(R) }.

lexeme(R, P) --> ws(_), { ! }, lexeme(R, P).
lexeme(R, P) --> call(R, P).

lexeme(R, O, P) --> ws(_), { ! }, lexeme(R, O, P).
lexeme(R, O, P) --> call(R, O, P).

lexeme(R, O, U, P) --> ws(_), { ! }, lexeme(R, O, U, P).
lexeme(R, O, U, P) --> call(R, O, U, P).

ws(span(N,N)) --> [(N,C)], { char_type(C, space) }.

pos(span(S,E), span(0,0), span(S,E)) :- !.
pos(span(S,_), span(_,E), span(S,E)) :- !.
pos(S, span(0,0), span(S,S)) :- !.
pos(S, span(_,E), span(S,E)).
