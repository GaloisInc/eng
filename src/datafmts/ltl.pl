% Parses the LTL input text into an AST.

:- module(ltl, [ parse_ltl/2,
                 past_optimize/2,
                 emit_ltl/2,
                 emit_CoCoSpec/2 ]).

:- use_module('../englib').


parse_ltl(Inp, AST) :-
    string_chars(Inp, CS),
    phrase(ltl(AST), CS, R), !,
    ( R == []
    -> true
    ; format('  LTL AST: ~w~n  REMAINDER: ~w~n', [ AST, R ])
    ).


ltl(E) --> boolEx(E).

arithEx(E) --> arithTerm(T), arithExMore(T, E).
%% ltl(expo(E, E)) --> lxm(ltl, E), lxm(expt), lxm(ltl, E).
arithTerm(neg(E)) --> minus(_), lxm(arith, E).
%% ltl(mul(E, E)) --> lxm(ltl, E), lxm(mult), lxm(ltl, E).
%% ltl(div(E, E)) --> lxm(ltl, E), lxm(div), lxm(ltl, E).
%% ltl(mod(E, E)) --> lxm(ltl, E), lxm(mod), lxm(ltl, E).
%% ltl(sub(E, E)) --> lxm(ltl, E), lxm(minus), lxm(ltl, E).
%% ltl(negfloatnum(M,E)) --> lxm(minus), number(M), ['.'], number(E).
%% ltl(floatnum(M,E)) --> lxm(number, M), ['.'], number(E).
%% ltl(negnum(N)) --> lxm(minus), number(N).
%% ltl(num(N)) --> lxm(number, N).
%% ltl(call(I, Args)) --> lxm(ident, I), args(Args).
arithTerm(val(N)) --> lxm(num, N).
arithTerm(id(I)) --> lxm(ident, I).
%% ltl(E) --> boolExpr(E, Ls0, Ls).
%% %% ltl(E) --> boolEx(E).
arithTerm(E) --> lxm(lp), lxm(arithEx, E), lxm(rp).
arithExMore(LT, Expr) --> lxm(expt), arithTerm(E), arithExMore(expo(LT, E), Expr).
arithExMore(LT, Expr) --> lxm(plus), arithTerm(E),
                          { optimize(add(LT, E), OptE) },
                          arithExMore(OptE, Expr).
arithExMore(LT, LT) --> [].

args([A|MA]) --> lxm(lp), ltl(A), moreArgs(MA), lxm(rp).
%% args([A|MA]) --> lxm(lp), boolExpr(A, Ls0, Ls), moreArgs(MA), lxm(rp).
args([A|MA]) --> lxm(lp), boolEx(A), moreArgs(MA), lxm(rp).
args([A]) --> lxm(lp), ltl(A), lxm(rp).
%% args([A]) --> lxm(lp), boolExpr(A, Ls0, Ls), lxm(rp).
args([A]) --> lxm(lp), boolEx(A), lxm(rp).

moreArgs([A|MA]) --> [','], ltl(A), moreArgs(MA).
moreArgs([A|MA]) --> [','], boolEx(A), moreArgs(MA).
moreArgs([A]) --> [','], ltl(A).
moreArgs([A]) --> [','], boolEx(A).

boolEx(E) --> boolTerm(T), boolExMore(T, E).
% boolTerm(boolcall(I,Args)) --> lxm(ident, I), args(Args). % intercepts others and recurses infinitely
boolTerm(true) --> lxm(w, "TRUE").
boolTerm(false) --> lxm(w, "FALSE").
boolTerm(boolid(I)) --> lxm(ident, I).

boolTerm(ltlH(E)) --> lxm(ltlH), lxm(boolEx, E).
boolTerm(ltlO(E)) --> lxm(ltlO), lxm(boolEx, E).
boolTerm(ltlG(E)) --> lxm(ltlG), lxm(boolEx, E).
boolTerm(ltlF(E)) --> lxm(ltlF), lxm(boolEx, E).
boolTerm(ltlY(E)) --> lxm(ltlY), lxm(boolEx, E).
boolTerm(ltlX(E)) --> lxm(ltlX), lxm(boolEx, E).
boolTerm(ltlZ(E)) --> lxm(ltlZ), lxm(boolEx, E).
boolTerm(ltlBefore(E)) --> lxm(ltlBefore), lxm(boolEx, E).
boolTerm(ltlAfter(E)) --> lxm(ltlAfter), lxm(boolEx, E).
boolTerm(not(E)) --> lxm(not), boolTerm(E).
boolTerm(E) --> lxm(lp), lxm(boolEx, E), lxm(rp).
boolTerm(eq(E1,E2)) --> lxm(arithEx, E1), lxm(eq), lxm(arithEx, E2).
boolTerm(le(E1,E2)) --> lxm(arithEx, E1), lxm(le), lxm(arithEx, E2).
boolTerm(ge(E1,E2)) --> lxm(arithEx, E1), lxm(ge), lxm(arithEx, E2).
boolTerm(lt(E1,E2)) --> lxm(arithEx, E1), lxm(lt), lxm(arithEx, E2).
boolTerm(gt(E1,E2)) --> lxm(arithEx, E1), lxm(gt), lxm(arithEx, E2).
boolTerm(neq(E1,E2)) --> lxm(arithEx, E1), lxm(neq), lxm(arithEx, E2).
boolTerm(next(E,E2)) --> lxm(next), boolEx(E), lxm(comma), boolEx(E2).
boolTerm(prev(E,E2)) --> lxm(prev), boolEx(E), lxm(comma), boolEx(E2).
boolExMore(LT, Expr) --> lxm(and), boolTerm(E), boolExMore(and(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(or), boolTerm(E), boolExMore(or(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(xor), boolTerm(E), boolExMore(xor(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(implies), boolTerm(E), boolExMore(implies(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(equiv), boolTerm(E), boolExMore(equiv(LT, E), Expr).

boolExMore(LT, Expr) --> lxm(ltlSI), lxm(bound, B), boolTerm(E),
                         boolExMore(binSI_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlSI), boolTerm(E),
                         boolExMore(binSI(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlS), lxm(bound, B), boolTerm(E),
                         boolExMore(binS_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlS), boolTerm(E),
                         boolExMore(binS(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlT), lxm(bound, B), boolTerm(E),
                         boolExMore(binT_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlT), boolTerm(E),
                         boolExMore(binT(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlUI), lxm(bound, B), boolTerm(E),
                         boolExMore(binUI_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlUI), boolTerm(E),
                         boolExMore(binUI(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlU), lxm(bound, B), boolTerm(E),
                         boolExMore(binU_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlU), boolTerm(E),
                         boolExMore(binU(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlV), lxm(bound, B), boolTerm(E),
                         boolExMore(binV_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlV), boolTerm(E),
                         boolExMore(binV(LT, E), Expr).
boolExMore(LT, LT) --> [].


bound(range2(B,E)) --> ['['], ltl(B), [','], ltl(E), [']'].
bound(range1(B)) --> ['['], ltl(B), [']'].
bound(salt_eq(E)) --> lxm(eq), lxm(ltl, E).
bound(salt_le(E)) --> lxm(le), lxm(ltl, E).
bound(salt_ge(E)) --> lxm(ge), lxm(ltl, E).
bound(salt_lt(E)) --> lxm(lt), lxm(ltl, E).
bound(salt_gt(E)) --> lxm(gt), lxm(ltl, E).
bound(salt_neq(E)) --> lxm(eq), lxm(ltl, E).

comma() --> [ ',' ].
lp() --> [ '(' ].
rp() --> [ ')' ].
expt() --> [ '^' ].
mult() --> [ '*' ].
div() --> [ '/' ].
plus() --> [ '+' ].
minus(m) --> [ '-' ].
not() --> [ '!' ].
and() --> [ '&' ].
or() --> [ '|' ].
xor() --> [ 'x', 'o', 'r' ].
xor() --> [ 'X', 'o', 'r' ].
xor() --> [ 'x', 'O', 'R' ].
xor() --> [ 'x', 'O', 'r' ].
xor() --> [ 'X', 'O', 'R' ].
mod() --> [ 'm', 'o', 'd' ].
mod() --> [ 'M', 'o', 'd' ].
mod() --> [ 'M', 'O', 'D' ].
eq() --> [ '=' ].
lt() --> [ '<' ].
gt() --> [ '>' ].
neq() --> [ '!', '=' ].
le() --> [ '<', '=' ].
ge() --> [ '>', '=' ].
implies() --> [ '-', '>' ].
equiv() --> [ '<', '-', '>' ].
next() --> lxm(w, "at"), lxm(w, "the"), lxm(w, "next"),
           lxm(w, "occurrence"), lxm(w, "of").
prev() --> lxm(w, "at"), lxm(w, "the"), lxm(w, "previous"),
           lxm(w, "occurrence"), lxm(w, "of").
ltlH() --> [ 'H' ], ws_.
ltlO() --> [ 'O' ].
ltlG() --> [ 'G' ].
ltlF() --> [ 'F' ].
ltlSI() --> [ 'S', 'I' ].
ltlS() --> [ 'S' ].
ltlT() --> [ 'T' ], ws_.
ltlUI() --> [ 'U', 'I' ].
ltlU() --> [ 'U' ].
ltlV() --> [ 'V' ].
ltlY() --> [ 'Y' ], ws_.
ltlX() --> [ 'X' ].
ltlZ() --> [ 'Z' ], ws_.
ltlBefore() --> [ '<', '|' ].
ltlAfter() --> [ '|', '>' ].


ident(I) --> w(I).  % Ident should allow $ and should not start with a numeric

lxm(R) --> ws_, { ! }, lxm(R).
lxm(R) --> call(R).

lxm(R, P) --> ws_, { ! }, lxm(R, P).
lxm(R, P) --> call(R, P).

lxm(R, O, P) --> ws_, { ! }, lxm(R, O, P).
lxm(R, O, P) --> call(R, O, P).

lxm(R, O, U, P) --> ws_, { ! }, lxm(R, O, U, P).
lxm(R, O, U, P) --> call(R, O, U, P).

%% wsp(P) --> ws(A), ws(B), { pos(A,B,P) }.
%% wsp(P) --> ws(P).

ws_() --> [C], { char_type(C, space) }.

w(W) --> [C], { wchar(C) }, w_(CS), { string_chars(W, [C|CS]) }.
w_([C|CS]) --> [C], { wchar(C) }, !, w_(CS).
w_([]) --> [].

wchar(C) :- \+ char_type(C, space),
            % Exclude things that might be individual tokens needing to be
            % recognized elsewhere in the grammar.
            \+ member(C, ['(', ')', '.', '!', '?', ':', ',',
                          %% '{', '}', '^', '[', ']', %% XXX?
                          '$',
                          '/']).

num([N|NS]) --> digit(N), num(NS).
num(N) --> digit(N).
digit(D) --> [ (D) ], { char_type(D, digit) }.


% ----------------------------------------------------------------------

past_optimize(and(E1, E2), and(E1O, E2O)) :-
    !, past_optimize(E1, E1O), past_optimize(E2, E2O).
past_optimize(or(E1, E2), or(E1O, E2O)) :-
    !, past_optimize(E1, E1O), past_optimize(E2, E2O).
past_optimize(implies(E1, E2), implies(E1O, E2O)) :-
    !, past_optimize(E1, E1O), past_optimize(E2, E2O).
past_optimize(ltlH(E), ltlH(EO)) :- !, past_optimize(E, EO).
past_optimize(ltlZ(false), not(ltlY(true))) :- !.  % R8, R1
past_optimize(AST, AST).

% ----------------------------------------------------------------------

emit_ltl(AST, LTLText) :-
    string_concat("LTLNo! ", AST, LTLText).  % TODO

% --------------------

% emit_CoCoSpec(I, O) :- string_concat("CoCoNo! ", I, O). % TODO
emit_CoCoSpec(true, true).
emit_CoCoSpec(false, false).
emit_CoCoSpec(val(V), V).
emit_CoCoSpec(boolid(N), N).
emit_CoCoSpec(id(I), I).
emit_CoCoSpec(not(E), C) :- emit_CoCoSpec(E, ES),
                          format(atom(CA), "not (~w)", [ES]),
                          atom_string(CA, C).
emit_CoCoSpec(neg(E), C) :- emit_CoCoSpec(E, ES),
                            format(atom(CA), "-(~w)", [ES]),
                            atom_string(CA, C).
emit_CoCoSpec(ltlH(E), C) :- coco_call("H", E, C).  % Historically
emit_CoCoSpec(ltlO(E), C) :- coco_call("O", E, C).  % Once
emit_CoCoSpec(ltlY(E), C) :- coco_call("YtoPre", E, C).  % PrevFalse
emit_CoCoSpec(ltlZ(E), C) :- coco_call("ZtoPre", E, C).  % PrevTrue

emit_CoCoSpec(and(E1, E2), C) :- coco_infix("and", E1, E2, C).
emit_CoCoSpec(or(E1, E2), C) :- coco_infix("or", E1, E2, C).
emit_CoCoSpec(xor(E1, E2), C) :- coco_infix("xor", E1, E2, C).
emit_CoCoSpec(implies(E1, E2), C) :- coco_infix("=>", E1, E2, C).
emit_CoCoSpec(equiv(E1, E2), C) :- coco_infix("=", E1, E2, C).
emit_CoCoSpec(eq(E1, E2), C) :- coco_infix("=", E1, E2, C).
emit_CoCoSpec(neq(E1, E2), C) :- coco_infix("<>", E1, E2, C).
emit_CoCoSpec(lt(E1, E2), C) :- coco_infix("<", E1, E2, C).
emit_CoCoSpec(le(E1, E2), C) :- coco_infix("<=", E1, E2, C).
emit_CoCoSpec(gt(E1, E2), C) :- coco_infix(">", E1, E2, C).
emit_CoCoSpec(ge(E1, E2), C) :- coco_infix(">=", E1, E2, C).
emit_CoCoSpec(binS(E1, E2), C) :- coco_call("SI", E1, E2, C). % SinceInclusive  % KWQ: Reversed (LTLASTSemantics.js)
emit_CoCoSpec(X, _) :- format('No CoCo conversion for: ~w~n', [X]), fail.

coco_call(F,A,C) :- emit_CoCoSpec(A,AS),
                    format(atom(CA), "~w(~w)", [F,AS]),
                    atom_string(CA, C).
coco_call(F,A,B,C) :- emit_CoCoSpec(A,AS),
                      emit_CoCoSpec(B,BS),
                      format(atom(CA), "~w(~w, ~w)", [F,AS,BS]),
                      atom_string(CA, C).
coco_infix(F,A,B,C) :- emit_CoCoSpec(A,AS),
                       emit_CoCoSpec(B,BS),
                       format(atom(CA), "(~w ~w ~w)", [AS,F,BS]),
                       atom_string(CA, C).
