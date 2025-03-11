% Parses the FRETtish statements supported by the NASA Fret tool
% (https://github.com/NASA-SW-VnV/fret).

:- module(frettish, [ parse_fret/3 ]).

:- use_module('../englib').

%% Parses a Frettish English requirement to a Fret structured requirement, using
%% the definitions and templates provided to enhance the structured requirement.
parse_fret(Context, English, FretRequirement) :-
    string_chars(English, ECodes),
    enumerate(ECodes, Input),
    phrase(frettish(Semantics), Input, Remaining),
    ( Remaining == []
    -> true
    ; ( Remaining = [(_,'.')]
      -> true
      ; show_remaining(Context, Remaining)
      )
    ),
    FretRequirement = Semantics.

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
                  timing_info(Timing),
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
    timing(Timing),
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
                  timing_info(Timing),
                  response_info(Responses, RespVars)
                 )) -->
    scope(Scope, ScopeVars),
    %% {format('....scope: ~w~n', [Scope])},
    conditions(Condition, CondVars),
    %% {format('....conditions: ~w with ~w~n', [Condition, CondVars])},
    lexeme(shall, _),
    !,
    component(Comp),
    %% {format('....component: ~w~n', [Comp])},
    timing(Timing),
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

scope_(_{scope:_{type: "in"}, scope_mode:Mode}, Vars, Pos) -->
    in(P0), scope_mode(Mode, Vars, PM), { pos(P0, PM, Pos) }.
scope_(_{scope:{type: "in"}, scope_mode:Mode}, Vars, Pos) -->
    during(P0), scope_mode(Mode, Vars, PM), { pos(P0, PM, Pos) }.
% scope_(TODO) --> TODO.

scope_mode(Mode, [Mode], P) --> lexeme(mode, P0), lexeme(word, Mode, PE),
                                { pos(P0, PE, P) }.
scope_mode(Mode, [Mode], P) --> lexeme(word, Mode, P0), lexeme(mode, PE),
                                { pos(P0, PE, P) }.
scope_mode(Mode, [Mode], P) --> lexeme(word, Mode, P).

% --------------------------------------------------

conditions(ReqCond, Vars) -->
    and(P0), cond_(C,CP,AllVars),
    { pos(P0, CP, P),
      range(P, Range),
      list_to_set(AllVars, Vars),
      put_dict(C, _{condition:"regular", conditionTextRange:Range}, ReqCond )
    }.
conditions(ReqCond, Vars) -->
    cond_(C,CP,AllVars),
    { range(CP, Range),
      list_to_set(AllVars, Vars),
      put_dict(C, _{condition:"regular", conditionTextRange:Range}, ReqCond )
    }.
conditions(_{condition:"null"}, []) --> [].

cond_(C,CP,V) -->
    qcond1_(C0,CP0,V0), opt_comma(_), qcond2_(C0,CP0,V0,C,CP,V), opt_comma(_).
cond_(C,CP,V) --> qcond1_(C,CP,V), opt_comma(_).

qcond1_(C,P,Vars) -->
    lexeme(qualifier, Q, QP),
    lexeme(precond, E, Vars, _), lexeme(is, _), lexeme(true, EP),
    { !,
      pos(QP, EP, P),
      qcond1_true_(Q,E,C)
    }.
qcond1_(C,P,Vars) -->
    lexeme(qualifier, Q, QP),
    lexeme(precond, E, Vars, _), lexeme(is, _), lexeme(false, EP),
    { !,
      pos(QP, EP, P),
      format(atom(PCA), "(!(~w))", [E]), atom_string(PCA, PC), % n.b. negated
      C = _{ qualifier_word:Q,
             pre_condition: PC,
             regular_condition: PC
           }
    }.
qcond1_(C,P,Vars) -->
    lexeme(qualifier, Q, QP), lexeme(precond, E, Vars, CP),
    { pos(QP, CP, P),
      qcond1_true_(Q,E,C)
    }.
qcond1_true_(Q,E,C) :-
    format(atom(PCA), "(~w)", [E]), atom_string(PCA, PC),
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

precond(E, V, P) --> bool_expr(E, V, P).

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

timing(_{ timing: "immediately", timingTextRange:Range}) -->
    lexeme(immediately, P),
    { range(P, Range) }.
timing(_{ timing: "eventually", timingTextRange:Range}) -->
    lexeme(eventually, P),
    { range(P, Range) }.
timing(_{ timing: "next", timingTextRange:Range}) -->
    lexeme(at, SP), lexeme(the, _), lexeme(next, _), lexeme(timepoint, TP),
    { pos(SP, TP, P), range(P, Range) }.
timing(_{ timing: "always"}) --> [],
                                 { writeln('warning, defaulting to always timing: may not have understood timing phrase') }.


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

bool_expr(V, Vars, P) --> bool_term(LT, LV, LP),
                          bool_exprMore(LT, LV, LP, V, Vars, P).
bool_term(V, [], P) --> lexeme(num, V, P).
bool_term(V, [V], P) --> lexeme(word, V, P).
bool_term(V, Vars, P) --> lexeme(lparen, LP),
                          bool_expr(PV, Vars, _),
                          { format(atom(X), '(~w)', [PV]), atom_string(X, V) },
                          lexeme(rparen, RP),
                          { pos(LP, RP, P) }.
bool_exprMore(LT, LV, LP, V, Vars, P) -->
    bool_exprMoreBin(LT, LV, LP, and_, "&", V, Vars, P).
bool_exprMore(LT, LV, LP, V, Vars, P) -->
    bool_exprMoreBin(LT, LV, LP, gteq_, ">=", V, Vars, P).
bool_exprMore(LT, LV, LP, V, Vars, P) -->
    bool_exprMoreBin(LT, LV, LP, lteq_, "<=", V, Vars, P).
bool_exprMore(LT, LV, LP, V, Vars, P) -->
    bool_exprMoreBin(LT, LV, LP, gt_, ">", V, Vars, P).
bool_exprMore(LT, LV, LP, V, Vars, P) -->
    bool_exprMoreBin(LT, LV, LP, lt_, "<", V, Vars, P).
bool_exprMore(LT, LV, LP, LT, LV, LP) --> [].

bool_exprMoreBin(LT, LV, LP, Matcher, Op, V, Vars, P) -->
    lexeme(Matcher, _), bool_term(RT, RV, RP),
    { binary_(Op, LT, LV, LP, RT, RV, RP, XS, XV, XP) },
    bool_exprMore(XS, XV, XP, V, Vars, P).

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

and(P) --> token("and", P).
at(P) --> token("at", P).
during(P) --> token("during", P).
eventually(P) --> token("eventually", P).
false(P) --> token("false", P).
if(P) --> token("if", P).
immediately(P) --> token("immediately", P).
in(P) --> token("in", P).
is(P) --> token("is", P).
mode(P) --> token("mode", P).
next(P) --> token("next", P).
occurrence(P) --> token("occurrence", P).
of(P) --> token("of", P).
or(P) --> token("or", P).
satisfy(P) --> token("satisfy", P).
shall(P) --> token("shall", P).
the(P) --> token("the", P).
timepoint(P) --> token("timepoint", P).
true(P) --> token("true", P).
unless(P) --> token("unless", P).
upon(P) --> token("upon", P).
whenever(P) --> token("whenever", P).
when(P) --> token("when", P).
where(P) --> token("where", P).

lparen(span(P,P)) --> [(P,'(')].
rparen(span(P,P)) --> [(P,')')].
gteq_(span(P,P)) --> [(P,'>'), (_, '=')].
lteq_(span(P,P)) --> [(P,'<'), (_, '=')].
gt_(span(P,P)) --> [(P,'>')].
lt_(span(P,P)) --> [(P,'<')].
and_(span(P,P)) --> [(P,'&')].
or_(span(P,P)) --> [(P,'|')].

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

number([N|NS]) --> digit(N), number(NS).
number(N) --> digit(N).
digit(D) --> [ D ], { member(D, "0123456789") }.  % KWQ: char_code numeric

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
