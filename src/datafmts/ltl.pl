% Parses the LTL input text into an AST.

:- module(ltl, [ parse_ltl/2,
                 emit_ltl/2,
                 fmap/3,
                 emit_CoCoSpec/2 ]).

:- use_module('../englib').


parse_ltl(Inp, AST) :-
    string_chars(Inp, CS),
    phrase(ltl(RawAST), CS, R), !,
    (fmap(optimize, RawAST, AST) ; AST = RawAST),
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
arithTerm(val(N)) --> lxm(num, N).
%% ltl(E) --> boolExpr(E, Ls0, Ls).
%% %% ltl(E) --> boolEx(E).
arithTerm(E) --> lxm(lp), lxm(arithEx, E), lxm(rp).
arithTerm(call(I, Args)) --> lxm(ident, I), lp, opArgs(Args), lxm(rp).
arithTerm(id(I)) --> lxm(ident, I).
arithExMore(LT, Expr) --> lxm(expt), arithTerm(E), arithExMore(expo(LT, E), Expr).
arithExMore(LT, Expr) --> lxm(plus), arithTerm(E), arithExMore(add(LT, E), Expr).
arithExMore(LT, LT) --> [].

boolEx(E) --> boolTerm(T), boolExMore(T, E).
boolTerm(true) --> lxm(w, "TRUE").
boolTerm(false) --> lxm(w, "FALSE").

boolTerm(ltlH_bound(B, E)) --> ['H'], bound(B), lxm(boolEx, E).
boolTerm(ltlH(E)) --> lxm(ltlH), lxm(boolEx, E).
boolTerm(ltlO_bound(B, E)) --> ['O'], bound(B), lxm(boolEx, E).
boolTerm(ltlO(E)) --> lxm(ltlO), lxm(boolEx, E).
boolTerm(ltlG_bound(B, E)) --> ['G'], bound(B), lxm(boolEx, E).
boolTerm(ltlG(E)) --> lxm(ltlG), lxm(boolEx, E).
boolTerm(ltlF_bound(B, E)) --> ['F'], bound(B), lxm(boolEx, E).
boolTerm(ltlF(E)) --> lxm(ltlF), lxm(boolEx, E).
boolTerm(ltlBefore_bound(B, E)) --> lxm(ltlBefore), bound(B), lxm(boolEx, E).
boolTerm(ltlBefore(E)) --> lxm(ltlBefore), lxm(boolEx, E).
boolTerm(ltlAfter_bound(B, E)) --> lxm(ltlAfter), bound(B), lxm(boolEx, E).
boolTerm(ltlAfter(E)) --> lxm(ltlAfter), lxm(boolEx, E).

boolTerm(ltlY(E)) --> lxm(ltlY), lxm(boolEx, E).
boolTerm(ltlX(E)) --> lxm(ltlX), lxm(boolEx, E).
boolTerm(ltlZ(E)) --> lxm(ltlZ), lxm(boolEx, E).

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
boolTerm(boolcall(I,Args)) --> lxm(ident, I), lp, opArgs(Args), lxm(rp).
boolTerm(boolid(I)) --> lxm(ident, I).

opArgs([Arg|Args]) --> oneArg(Arg), lxm(comma), opArgs(Args).
opArgs([Arg]) --> oneArg(Arg).
opArgs([]) --> [].

oneArg(A) --> boolEx(A).
oneArg(A) --> arithEx(A).


boolExMore(LT, Expr) --> lxm(and), boolTerm(E), boolExMore(and(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(or), boolTerm(E), boolExMore(or(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(xor), boolTerm(E), boolExMore(xor(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(implies), boolTerm(E), boolExMore(implies(LT, E), Expr).
boolExMore(LT, Expr) --> lxm(equiv), boolTerm(E), boolExMore(equiv(LT, E), Expr).

boolExMore(LT, Expr) --> ['S', 'I'], bound(B), boolTerm(E),
                         boolExMore(ltlSI_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlSI), boolTerm(E),
                         boolExMore(ltlSI(LT, E), Expr).
boolExMore(LT, Expr) --> ['S'], bound(B), boolTerm(E),
                         boolExMore(ltlS_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlS), boolTerm(E),
                         boolExMore(ltlS(LT, E), Expr).
boolExMore(LT, Expr) --> ['T'], bound(B), boolTerm(E),
                         boolExMore(ltlT_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlT), boolTerm(E),
                         boolExMore(ltlT(LT, E), Expr).
boolExMore(LT, Expr) --> ['U', 'I'], bound(B), boolTerm(E),
                         boolExMore(ltlUI_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlUI), boolTerm(E),
                         boolExMore(ltlUI(LT, E), Expr).
boolExMore(LT, Expr) --> ['U'], bound(B), boolTerm(E),
                         boolExMore(ltlU_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlU), boolTerm(E),
                         boolExMore(ltlU(LT, E), Expr).
boolExMore(LT, Expr) --> ['V'], bound(B), boolTerm(E),
                         boolExMore(ltlV_bound(B, LT, E), Expr).
boolExMore(LT, Expr) --> lxm(ltlV), boolTerm(E),
                         boolExMore(ltlV(LT, E), Expr).
boolExMore(LT, LT) --> [].


bound(Bnd) --> ['['], range(Bnd), ws_, [']'].
bound(Bnd) --> ['['], range(Bnd), [']'].
bound(Bnd) --> ['['], saltBound(Bnd), [']'].
bound(Bnd) --> ['['], saltBound(Bnd), ws_, [']'].

range(range2(B,E)) --> arithEx(B), [','], arithEx(E).
range(range1(B)) --> arithEx(B).
saltBound(salt_eq(E)) --> lxm(eq), lxm(arithEx, E).
saltBound(salt_le(E)) --> lxm(le), lxm(arithEx, E).
saltBound(salt_ge(E)) --> lxm(ge), lxm(arithEx, E).
saltBound(salt_lt(E)) --> lxm(lt), lxm(arithEx, E).
saltBound(salt_gt(E)) --> lxm(gt), lxm(arithEx, E).
saltBound(salt_neq(E)) --> lxm(neq), lxm(arithEx, E).

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
% n.b. temporal operators are normally followed by whitespace to differentiate
% between them and a user variable (e.g. "Hot" should not be parsed as "H ot")
ltlH() --> [ 'H' ], ws_.
ltlO() --> [ 'O' ], ws_.
ltlG() --> [ 'G' ], ws_.
ltlF() --> [ 'F' ], ws_.
ltlSI() --> [ 'S', 'I' ], ws_.
ltlS() --> [ 'S' ], ws_.
ltlT() --> [ 'T' ], ws_.
ltlUI() --> [ 'U', 'I' ], ws_.
ltlU() --> [ 'U' ], ws_.
ltlV() --> [ 'V' ], ws_.
ltlY() --> [ 'Y' ], ws_.
ltlX() --> [ 'X' ], ws_.
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

w(W) --> [C], { wchar1(C) }, w_(CS), { string_chars(W, [C|CS]) }.
w_([C|CS]) --> [C], { wchar(C) }, !, w_(CS).
w_([]) --> [].

wchar1(C) :- wchar(C),
             \+ member(C, ['1','2','3','4','5','6','7','8','9','0']).
wchar(C) :- \+ char_type(C, space),
            % Exclude things that might be individual tokens needing to be
            % recognized elsewhere in the grammar.
            \+ member(C, ['(', ')', '.', '!', '?', ':', ',',
                          '{', '}', '^', '[', ']', %% XXX?
                          '<', '>', '=',
                          '$',
                          '/']).

num(V) --> digit(N), num(NS), V is (N * 10) + NS.
num(N) --> digit(N).
digit(D) --> [ (C) ], { char_type(C, digit),
                        atom_codes(C, [CS]),
                        atom_codes('0', [ZS]),
                        plus(ZS, D, CS)
                      }.


% ------------------------------------------------------------

% fmap walks an AST, depth first calling the supplied Op for each element.  The
% Op should return (as the second parameter) any desired element
% modification---or just the passed element by default.

fmap(Op, val(N), V) :- call(Op, val(N), V).
fmap(Op, id(I), O) :- call(Op, id(I), O).
fmap(Op, boolid(I), O) :- call(Op, boolid(I), O).
fmap(Op, true, V) :- call(Op, true, V).
fmap(Op, false, V) :- call(Op, false, V).
fmap(Op, neg(E), O) :- fmap(Op, E, I), call(Op, neg(I), O).
fmap(Op, call(I, Args), O) :- call(Op, callid(I), callid(OI)),
                              maplist(fmap(Op), Args, OArgs),
                              call(Op, call(OI, OArgs), O).
fmap(Op, expo(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                           call(Op, expo(OL, OR), O).
fmap(Op, add(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                          call(Op, add(OL, OR), O).
fmap(Op, ltlH_bound(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                                 call(Op, ltlH_bound(OL, OR), O).
fmap(Op, ltlO_bound(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                                 call(Op, ltlO_bound(OL, OR), O).
fmap(Op, ltlG_bound(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                                 call(Op, ltlG_bound(OL, OR), O).
fmap(Op, ltlF_bound(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                                 call(Op, ltlF_bound(OL, OR), O).
fmap(Op, ltlBefore_bound(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                                 call(Op, ltlBefore_bound(OL, OR), O).
fmap(Op, ltlAfter_bound(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                                     call(Op, ltlAfter_bound(OL, OR), O).
fmap(Op, ltlH(L), O) :- fmap(Op, L, OL), call(Op, ltlH(OL), O).
fmap(Op, ltlO(L), O) :- fmap(Op, L, OL), call(Op, ltlO(OL), O).
fmap(Op, ltlG(L), O) :- fmap(Op, L, OL), call(Op, ltlG(OL), O).
fmap(Op, ltlF(L), O) :- fmap(Op, L, OL), call(Op, ltlF(OL), O).
fmap(Op, ltlBefore(L), O) :- fmap(Op, L, OL), call(Op, ltlBefore(OL), O).
fmap(Op, ltlAfter(L), O) :- fmap(Op, L, OL), call(Op, ltlAfter(OL), O).
fmap(Op, ltlY(L), O) :- fmap(Op, L, OL), call(Op, ltlY(OL), O).
fmap(Op, ltlX(L), O) :- fmap(Op, L, OL), call(Op, ltlX(OL), O).
fmap(Op, ltlZ(L), O) :- fmap(Op, L, OL), call(Op, ltlZ(OL), O).
fmap(Op, ltlSI(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                            call(Op, ltlSI(OL, OR), O).
fmap(Op, ltlS(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                           call(Op, ltlS(OL, OR), O).
fmap(Op, ltlT(L), O) :- fmap(Op, L, OL), call(Op, ltlT(OL), O).
fmap(Op, ltlUI(L), O) :- fmap(Op, L, OL), call(Op, ltlUI(OL), O).
fmap(Op, ltlU(L), O) :- fmap(Op, L, OL), call(Op, ltlU(OL), O).
fmap(Op, ltlV(L), O) :- fmap(Op, L, OL), call(Op, ltlV(OL), O).
fmap(Op, not(E), O) :- fmap(Op, E, I), call(Op, not(I), O).
fmap(Op, eq(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                         call(Op, eq(OL, OR), O).
fmap(Op, le(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                         call(Op, le(OL, OR), O).
fmap(Op, ge(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                         call(Op, ge(OL, OR), O).
fmap(Op, lt(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                         call(Op, lt(OL, OR), O).
fmap(Op, gt(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                         call(Op, gt(OL, OR), O).
fmap(Op, neq(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                          call(Op, neq(OL, OR), O).
fmap(Op, next(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                           call(Op, next(OL, OR), O).
fmap(Op, prev(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                           call(Op, prev(OL, OR), O).
fmap(Op, boolcall(I, Args), O) :- call(Op, callid(I), callid(OI)),
                                  maplist(fmap(Op), Args, OArgs),
                                  call(Op, boolcall(OI, OArgs), O).
fmap(Op, and(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                          call(Op, and(OL, OR), O).
fmap(Op, or(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                         call(Op, or(OL, OR), O).
fmap(Op, xor(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                          call(Op, xor(OL, OR), O).
fmap(Op, implies(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                              call(Op, implies(OL, OR), O).
fmap(Op, equiv(L, R), O) :- fmap(Op, L, OL), fmap(Op, R, OR),
                            call(Op, equiv(OL, OR), O).
fmap(Op, range2(A, B), O) :- fmap(Op, A, OA), fmap(Op, B, OB),
                             call(Op, range2(OA, OB), O).
fmap(Op, salt_le(A), O) :- fmap(Op, A, OA), call(Op, salt_le(OA), O).
fmap(Op, salt_ge(A), O) :- fmap(Op, A, OA), call(Op, salt_ge(OA), O).
fmap(Op, salt_lt(A), O) :- fmap(Op, A, OA), call(Op, salt_lt(OA), O).
fmap(Op, salt_gt(A), O) :- fmap(Op, A, OA), call(Op, salt_gt(OA), O).
fmap(Op, salt_eq(A), O) :- fmap(Op, A, OA), call(Op, salt_eq(OA), O).
fmap(Op, salt_neq(A), O) :- fmap(Op, A, OA), call(Op, salt_neq(OA), O).
fmap(_, Elem, Elem) :-
    print_message(warning, no_fmap_for_elem(Elem)).

prolog:message(no_fmap_for_elem(Elem)) -->
    [ 'No fmap for ~w~n' - [Elem] ].

% ----------------------------------------------------------------------

optimize(add(val(X), val(Y)), val(V)) :- V is X + Y, !.
optimize(sub(val(X), val(Y)), val(V)) :- V is X - Y, !.
optimize(not(ltlY(true)), ltlZ(false)) :- !.
optimize(not(ltlZ(false)), ltlY(true)) :- !.
optimize(not(not(E)), E) :- !.
optimize(X, X).

%% past_optimize(and(E1, E2), and(E1O, E2O)) :-
%%     !, past_optimize(E1, E1O), past_optimize(E2, E2O).
%% past_optimize(or(E1, E2), or(E1O, E2O)) :-
%%     !, past_optimize(E1, E1O), past_optimize(E2, E2O).
%% past_optimize(implies(E1, E2), implies(E1O, E2O)) :-
%%     !, past_optimize(E1, E1O), past_optimize(E2, E2O).
%% past_optimize(ltlH(E), ltlH(EO)) :- !, past_optimize(E, EO).
%% past_optimize(ltlZ(false), not(ltlY(true))) :- !.  % R8, R1
%% past_optimize(AST, AST).

% ----------------------------------------------------------------------

emit_ltl(true, "TRUE").
emit_ltl(false, "FALSE").
emit_ltl(val(V), O) :- atom_string(V, O).
emit_ltl(boolid(I), I).
emit_ltl(id(I), I).
emit_ltl(ltlH(A), O) :- emit_ltl(A, AO),
                        format(atom(X), '(H ~w)', [ AO ]),
                        atom_string(X, O).
emit_ltl(ltlO(A), O) :- emit_ltl(A, AO),
                        format(atom(X), '(O ~w)', [ AO ]),
                        atom_string(X, O).
emit_ltl(ltlG(A), O) :- emit_ltl(A, AO),
                        format(atom(X), '(G ~w)', [ AO ]),
                        atom_string(X, O).
emit_ltl(ltlF(A), O) :- emit_ltl(A, AO),
                        format(atom(X), '(F ~w)', [ AO ]),
                        atom_string(X, O).
emit_ltl(ltlBefore(A), O) :- emit_ltl(A, AO),
                             format(atom(X), '(<| ~w)', [ AO ]),
                             atom_string(X, O).
emit_ltl(ltlAfter(A), O) :- emit_ltl(A, AO),
                            format(atom(X), '(|> ~w)', [ AO ]),
                            atom_string(X, O).
emit_ltl(ltlX(A), O) :- emit_ltl(A, AO),
                        format(atom(X), '(X ~w)', [ AO ]),
                        atom_string(X, O).
emit_ltl(ltlY(A), O) :- emit_ltl(A, AO),
                        format(atom(X), '(Y ~w)', [ AO ]),
                        atom_string(X, O).
emit_ltl(ltlZ(A), O) :- emit_ltl(A, AO),
                        format(atom(X), '(Z ~w)', [ AO ]),
                        atom_string(X, O).
emit_ltl(neg(A), O) :- emit_ltl(A, AO),
                       format(atom(X), '(- ~w)', [ AO ]),
                       atom_string(X, O).
emit_ltl(not(A), O) :- emit_ltl(A, AO),
                       format(atom(X), '(! ~w)', [ AO ]),
                       atom_string(X, O).
emit_ltl(add(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                          format(atom(X), '(~w + ~w)', [ AO, BO ]),
                          atom_string(X, O).
emit_ltl(and(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                          format(atom(X), '(~w & ~w)', [ AO, BO ]),
                          atom_string(X, O).
emit_ltl(eq(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                         format(atom(X), '(~w = ~w)', [ AO, BO ]),
                         atom_string(X, O).
emit_ltl(equiv(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                            format(atom(X), '(~w <=> ~w)', [ AO, BO ]),
                            atom_string(X, O).
emit_ltl(expo(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                          format(atom(X), '(~w ^ ~w)', [ AO, BO ]),
                          atom_string(X, O).
emit_ltl(ge(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                         format(atom(X), '(~w >= ~w)', [ AO, BO ]),
                         atom_string(X, O).
emit_ltl(gt(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                         format(atom(X), '(~w > ~w)', [ AO, BO ]),
                         atom_string(X, O).
emit_ltl(implies(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                              format(atom(X), '(~w -> ~w)', [ AO, BO ]),
                              atom_string(X, O).
emit_ltl(le(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                         format(atom(X), '(~w <= ~w)', [ AO, BO ]),
                         atom_string(X, O).
emit_ltl(lt(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                         format(atom(X), '(~w < ~w)', [ AO, BO ]),
                         atom_string(X, O).
emit_ltl(ltlS(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                           format(atom(X), '(~w S ~w)', [ AO, BO ]),
                           atom_string(X, O).
emit_ltl(ltlSI(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                            format(atom(X), '(~w SI ~w)', [ AO, BO ]),
                            atom_string(X, O).
emit_ltl(ltlT(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                           format(atom(X), '(~w T ~w)', [ AO, BO ]),
                           atom_string(X, O).
emit_ltl(ltlU(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                           format(atom(X), '(~w U ~w)', [ AO, BO ]),
                           atom_string(X, O).
emit_ltl(ltlUI(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                            format(atom(X), '(~w UI ~w)', [ AO, BO ]),
                            atom_string(X, O).
emit_ltl(ltlV(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                           format(atom(X), '(~w V ~w)', [ AO, BO ]),
                           atom_string(X, O).
emit_ltl(or(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                         format(atom(X), '(~w | ~w)', [ AO, BO ]),
                         atom_string(X, O).
emit_ltl(ltlH_bound(B, A), O) :- emit_ltl(B, BO), emit_ltl(A, AO),
                          format(atom(X), '(H~w ~w)', [ BO, AO ]),
                          atom_string(X, O).
emit_ltl(ltlO_bound(B, A), O) :- emit_ltl(B, BO), emit_ltl(A, AO),
                                 format(atom(X), '(O~w ~w)', [ BO, AO ]),
                                 atom_string(X, O).
emit_ltl(ltlG_bound(B, A), O) :- emit_ltl(B, BO), emit_ltl(A, AO),
                                 format(atom(X), '(G~w ~w)', [ BO, AO ]),
                                 atom_string(X, O).
emit_ltl(ltlF_bound(B, A), O) :- emit_ltl(B, BO), emit_ltl(A, AO),
                                 format(atom(X), '(F~w ~w)', [ BO, AO ]),
                                 atom_string(X, O).
emit_ltl(ltlBefore_bound(B, A), O) :- emit_ltl(B, BO), emit_ltl(A, AO),
                                      format(atom(X), '(<|~w ~w)', [ BO, AO ]),
                                      atom_string(X, O).
emit_ltl(ltlAfter_bound(B, A), O) :- emit_ltl(B, BO), emit_ltl(A, AO),
                                     format(atom(X), '(|>~w ~w)', [ BO, AO ]),
                                     atom_string(X, O).
emit_ltl(neq(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                         format(atom(X), '(~w != ~w)', [ AO, BO ]),
                         atom_string(X, O).
emit_ltl(next(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                           format(atom(X), 'at the next occurrence of ~w, ~w)', [ AO, BO ]),
                           atom_string(X, O).
emit_ltl(prev(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                           format(atom(X), 'at the previous occurrence of ~w, ~w)', [ AO, BO ]),
                           atom_string(X, O).
emit_ltl(range2(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                             format(atom(X), '[~w, ~w]', [ AO, BO ]),
                             atom_string(X, O).
emit_ltl(salt_lt(A), O) :- emit_ltl(A, AO),
                           format(atom(X), '[<~w ]', [ AO ]),
                           atom_string(X, O).
emit_ltl(salt_gt(A), O) :- emit_ltl(A, AO),
                           format(atom(X), '[>~w ]', [ AO ]),
                           atom_string(X, O).
emit_ltl(salt_le(A), O) :- emit_ltl(A, AO),
                           format(atom(X), '[<=~w ]', [ AO ]),
                           atom_string(X, O).
emit_ltl(salt_ge(A), O) :- emit_ltl(A, AO),
                           format(atom(X), '[>=~w ]', [ AO ]),
                           atom_string(X, O).
emit_ltl(salt_eq(A), O) :- emit_ltl(A, AO),
                           format(atom(X), '[=~w ]', [ AO ]),
                           atom_string(X, O).
emit_ltl(salt_neq(A), O) :- emit_ltl(A, AO),
                            format(atom(X), '[!=~w ]', [ AO ]),
                            atom_string(X, O).
emit_ltl(xor(A, B), O) :- emit_ltl(A, AO), emit_ltl(B, BO),
                          format(atom(X), '(~w XOR ~w)', [ AO, BO ]),
                          atom_string(X, O).

emit_ltl(X, "") :- print_message(error, no_LTL_emit_for(X)), fail.

prolog:message(no_LTL_emit_for(AST_Element)) -->
    [ 'No LTL text form for: ~w' - [AST_Element] ].


% --------------------

emit_CoCoSpec(true, true) :- !.
emit_CoCoSpec(false, false) :- !.
emit_CoCoSpec(val(V), V) :- !.
emit_CoCoSpec(boolid(N), N) :- !.
emit_CoCoSpec(id(I), I) :- !.
emit_CoCoSpec(not(E), C) :- emit_CoCoSpec(E, ES),
                            format(atom(CA), "not (~w)", [ES]),
                            atom_string(CA, C), !.
emit_CoCoSpec(neg(E), C) :- emit_CoCoSpec(E, ES),
                            format(atom(CA), "-(~w)", [ES]),
                            atom_string(CA, C), !.
emit_CoCoSpec(ltlH(E), C) :- coco_call("H", E, C), !.  % Historically
emit_CoCoSpec(ltlH_bound(B,E), C) :- coco_call("HT", B, E, C), !.
emit_CoCoSpec(ltlO(E), C) :- coco_call("O", E, C), !.  % Once
emit_CoCoSpec(ltlO_bound(B, E), C) :- coco_call("OT", B, E, C), !.  % Once
% n.b. can omit any future-time specs; CoCoSpec is only past-time.
emit_CoCoSpec(ltlY(E), C) :- coco_call("YtoPre", E, C), !.  % PrevFalse
emit_CoCoSpec(ltlZ(E), C) :- coco_call("ZtoPre", E, C), !.  % PrevTrue

emit_CoCoSpec(add(E1, E2), C) :- coco_infix("+", E1, E2, C), !.
emit_CoCoSpec(and(E1, E2), C) :- coco_infix("and", E1, E2, C), !.
emit_CoCoSpec(expo(E1, E2), C) :- coco_infix("^", E1, E2, C), !.
emit_CoCoSpec(or(E1, E2), C) :- coco_infix("or", E1, E2, C), !.
emit_CoCoSpec(xor(E1, E2), C) :- coco_infix("xor", E1, E2, C), !.
emit_CoCoSpec(implies(E1, E2), C) :- coco_infix("=>", E1, E2, C), !.
emit_CoCoSpec(equiv(E1, E2), C) :- coco_infix("=", E1, E2, C), !.
emit_CoCoSpec(eq(E1, E2), C) :- coco_infix("=", E1, E2, C), !.
emit_CoCoSpec(neq(E1, E2), C) :- coco_infix("<>", E1, E2, C), !.
emit_CoCoSpec(lt(E1, E2), C) :- coco_infix("<", E1, E2, C), !.
emit_CoCoSpec(le(E1, E2), C) :- coco_infix("<=", E1, E2, C), !.
emit_CoCoSpec(gt(E1, E2), C) :- coco_infix(">", E1, E2, C), !.
emit_CoCoSpec(ge(E1, E2), C) :- coco_infix(">=", E1, E2, C), !.

% Note: CoCoSpec reverses the arguments for: S, ST, SI, and SIT.  S(p,q) = q S p.
% See LTLASTSemantics.js.
emit_CoCoSpec(ltlS(E1, E2), C) :- coco_call("S", E2, E1, C), !. % Since
emit_CoCoSpec(ltlSI(E1, E2), C) :- coco_call("SI", E2, E1, C), !. % SinceInclusive

emit_CoCoSpec(range2(B, E), C) :- emit_CoCoSpec(B, BS),
                                  emit_CoCoSpec(E, ES),
                                  format(atom(CA), "~w, ~w", [ ES, BS ]),
                                  atom_string(CA, C), !.

emit_CoCoSpec(salt_eq(B), C) :- emit_CoCoSpec(B, BS),
                                format(atom(CA), "~w, ~w", [BS, BS]),
                                atom_string(CA, C), !.
emit_CoCoSpec(salt_le(B), C) :- emit_CoCoSpec(B, BS),
                                format(atom(CA), "~w, 0", [BS]),
                                atom_string(CA, C), !.
emit_CoCoSpec(salt_lt(B), C) :- emit_CoCoSpec(B, BS),
                                format(atom(CA), "(~w - 1), 0", [BS]),
                                atom_string(CA, C), !.


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
