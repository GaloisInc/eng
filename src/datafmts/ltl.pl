% Parses the LTL input text into an AST.

:- module(ltl, [ define_ltl_language/0,
                 parse_ltl/2,
                 emit_ltl/2
               ]).

:- use_module('../englib').
:- use_module('../exprlang').

define_ltl_language() :-
    ltl_langdef(LangDef),
    define_language(LangDef, _).

parse_ltl(Inp, AST) :-
    ltl_langdef(LangDef),
    parse_expr(LangDef, Inp, RawAST),
    get_dict(language, LangDef, Language),
    (fmap_abt(Language, ltl:optimize, RawAST, AST) ; AST = RawAST).

emit_ltl(AST, Text) :-
    ltl_langdef(LangDef),
    get_dict(language, LangDef, Language),
    emit_expr(Language, AST, Text).

% These ops are exported from exprlang, but apparently their precedence is lost
% (causing →(⦂(if, bool),a) instead of ⦂(if, →(bool,a)) as expected).  Redeclare
% their precedence here.
:- op(760, yfx, ⦂).
:- op(750, xfy, →).

ltl_langdef(
    langdef{
        language: ltl,
        types: [ number, bool ],
        atoms: [ ], % lit, num ],
        variable_ref: [ ident ],
        phrases:
        [ term(num ⦂ number, num, emit_simple_term(num)),
          term(lit ⦂ bool, [true]>>word('TRUE'),
               [term(lit(true), bool),'TRUE']>>true), %emit_simple_term(lit)),
          term(lit ⦂ bool, [false]>>word('FALSE'),
               [_,'FALSE']>>true), %emit_simple_term(lit)),
          term(ident ⦂ a, word, emit_simple_term(ident)),
          expop(not ⦂ bool → bool, [[]>>lexeme(chrs('!')), subexpr],
                [_,[A],T]>>fmt_str(T, '(! ~w)', [A])),
          expop(and ⦂ bool → bool → bool, infix(chrs('&')), emit_infix("&")),
          expop(or ⦂ bool → bool → bool, infix(chrs('|')), emit_infix("|")),
          expop(xor ⦂ bool → bool → bool, infix(tok(xor)), emit_infix("XOR")),
          expop(ltlF ⦂ bool → bool, [[]>>lexeme(word('F')), subexpr],
                  [_,[A],T]>>fmt_str(T, '(F ~w)', [A])),
          expop(ltlG ⦂ bool → bool, [[]>>lexeme(word('G')), subexpr],
                  [_,[A],T]>>fmt_str(T, '(G ~w)', [A])),
          expop(ltlH ⦂ bool → bool, [[]>>lexeme(word('H')), subexpr],
                  [_,[A],T]>>fmt_str(T, '(H ~w)', [A])),
          expop(ltlO ⦂ bool → bool, [[]>>lexeme(word('O')), subexpr],
                  [_,[A],T]>>fmt_str(T, '(O ~w)', [A])),
          expop(ltlS ⦂ bool → bool → bool, infix(chrs('S')), emit_infix("S")),
          expop(ltlSI ⦂ bool → bool → bool, infix(chrs('SI')), emit_infix("SI")),
          expop(ltlT ⦂ bool → bool → bool, infix(word('T')), emit_infix("T")),
          expop(ltlU ⦂ bool → bool → bool, infix(word('U')), emit_infix("U")),
          expop(ltlUI ⦂ bool → bool → bool, infix(word('UI')), emit_infix("UI")),
          expop(ltlV ⦂ bool → bool → bool, infix(word('V')), emit_infix("V")),
          expop(ltlO ⦂ bool → bool, [[]>>lexeme(word('O')), subexpr],
                  [_,[A],T]>>fmt_str(T, '(O ~w)', [A])),
          expop(ltlX ⦂ bool → bool, [[]>>lexeme(word('X')), subexpr],
                  [_,[A],T]>>fmt_str(T, '(X ~w)', [A])),
          expop(ltlY ⦂ bool → bool, [[]>>lexeme(word('Y')), subexpr],
                  [_,[A],T]>>fmt_str(T, '(Y ~w)', [A])),
          expop(ltlZ ⦂ bool → bool, [[]>>lexeme(word('Z')), subexpr],
                  [_,[A],T]>>fmt_str(T, '(Z ~w)', [A])),
          expop(ltlF_bound ⦂ range → bool → bool,
                [[]>>lexeme(chrs('F')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str(T, '(F~w ~w)', [R,A])),
          expop(ltlG_bound ⦂ range → bool → bool,
                [[]>>lexeme(chrs('G')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str(T, '(G~w ~w)', [R,A])),
          expop(ltlH_bound ⦂ range → bool → bool,
                [[]>>lexeme(chrs('H')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str(T, '(H~w ~w)', [R,A])),
          expop(ltlO_bound ⦂ range → bool → bool,
                [[]>>lexeme(chrs('O')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str(T, '(O~w ~w)', [R,A])),
          % TODO: these are LL non-productive and therefore cause infinite recursion.  But are they even valid?
          %% expop(ltlS_bound ⦂ bool → range → bool → bool,
          %%       [subexpr, []>>lexeme(chrs('SI')), subexpr, subexpr],
          %%       [_,[B,R,A],T]>>fmt_str(T, '(~w SI~w ~w)', [B,R,A])),
          %% expop(ltlU_bound ⦂ bool → range → bool → bool,
          %%       [subexpr, []>>lexeme(chrs('UI')), subexpr, subexpr],
          %%       [_,[B,R,A],T]>>fmt_str(T, '(~w UI~w ~w)', [B,R,A])),
          expop(before_bound ⦂ number → bool → bool,
                [[]>>lexeme(chrs('<|')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str('(<| ~w ~w)', [R, A])),
          expop(before ⦂ bool → bool,
                [[]>>lexeme(chrs('<|')), subexpr],
                [_,[A],T]>>fmt_str('(<| ~w)', [A])),
          expop(after_bound ⦂ number → bool → bool,
                [[]>>lexeme(chrs('|>')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str('(|> ~w ~w)', [R, A])),
          expop(after ⦂ bool → bool,
                [[]>>lexeme(chrs('|>')), subexpr],
                [_,[A],T]>>fmt_str('(|> ~w)', [A])),
          %% vv --- begin salt bounds specification
          expop(range_exact ⦂ number → range,
                [[]>>lexeme(chrs('[=')), subexpr, lexeme(chrs(']'))],
                [_,[R],T]>>fmt_str(T, '[=~w]', [R])),
          expop(range_max_incl ⦂ number → range,
                [[]>>lexeme(chrs('[<=')), subexpr, lexeme(chrs(']'))],
                [_,[R],T]>>fmt_str(T, '[<=~w]', [R])),
          expop(range_max ⦂ number → range,
                [[]>>lexeme(chrs('[<')), subexpr, lexeme(chrs(']'))],
                [_,[R],T]>>fmt_str(T, '[<~w]', [R])),
          expop(range_min_incl ⦂ number → range,
                [[]>>lexeme(chrs('[>=')), subexpr, lexeme(chrs(']'))],
                [_,[R],T]>>fmt_str(T, '[>=~w]', [R])),
          expop(range_min ⦂ number → range,
                [[]>>lexeme(chrs('[>')), subexpr, lexeme(chrs(']'))],
                [_,[R],T]>>fmt_str(T, '[>~w]', [R])),
          %% TODO: [!=   ... not sure now this is emitted in Lustre.  Needed?
          %% ^^ --- end salt bounds specification
          expop(range_min_max ⦂ number → number → range,
                [[]>>lexeme(chrs('[')), subexpr,
                 lexeme(chrs(',')), subexpr, lexeme(chrs(']'))],
                [_,[L,R],T]>>fmt_str(T, '[~w, ~w]', [L, R])),
          expop(implies ⦂ a → a → a, infix(chrs('->')), emit_infix("->")),
          expop(eq ⦂ a → a → bool, infix(chrs('=')), emit_infix("=")),
          expop(equiv ⦂ a → a → bool, infix(chrs('<=>')), emit_infix("<=>")),
          expop(equiv ⦂ a → a → bool, infix(chrs('<->')), emit_infix("<=>")),
          expop(neq ⦂ a → a → bool, infix(chrs('!=')), emit_infix("!=")),
          expop(gteq ⦂ number → number → bool, infix(chrs('>=')), emit_infix(">=")),
          expop(lteq ⦂ number → number → bool, infix(chrs('<=')), emit_infix("<=")),
          expop(gt ⦂ number → number → bool, infix(chrs('>')), emit_infix(">")),
          expop(lt ⦂ number → number → bool, infix(chrs('<')), emit_infix("<")),
          expop(neg ⦂ number → number, [[]>>lexeme(chrs('-')), subexpr],
                [_,[A],T]>>fmt_str(T, '(- ~w)', [A])),
          expop(add ⦂ number → number → number, infix(chrs('+')), emit_infix("+")),
          expop(sub ⦂ number → number → number, infix(chrs('-')), emit_infix("-")),
          expop(mul ⦂ number → number → number, infix(chrs('*')), emit_infix("*")),
          expop(divd ⦂ number → number → number, infix(chrs('/')), emit_infix("/")),
          expop(expo ⦂ number → number → number, infix(chrs('^')), emit_infix("^"))
               %% TODO: at the next occurrence of BOOL
               %% TODO: at the previous occurrence of BOOL
        ]}).


% ------------------------------------------------------------

% fmap walks an AST, depth first calling the supplied Op for each element.  The
% Op should return (as the second parameter) any desired element
% modification---or just the passed element by default.

%% KWQ: remove fmap_
%% fmap_(Op, val(N), V) :- call(Op, val(N), V).
%% fmap_(Op, id(I), O) :- call(Op, id(I), O).
%% fmap_(Op, boolid(I), O) :- call(Op, boolid(I), O).
%% fmap_(Op, true, V) :- call(Op, true, V).
%% fmap_(Op, false, V) :- call(Op, false, V).
%% fmap_(Op, neg(E), O) :- fmap_(Op, E, I), call(Op, neg(I), O).
%% fmap_(Op, call(I, Args), O) :- call(Op, callid(I), callid(OI)),
%%                               maplist(fmap_(Op), Args, OArgs),
%%                               call(Op, call(OI, OArgs), O).
%% fmap_(Op, expo(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                            call(Op, expo(OL, OR), O).
%% fmap_(Op, add(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                           call(Op, add(OL, OR), O).
%% fmap_(Op, ltlH_bound(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                                  call(Op, ltlH_bound(OL, OR), O).
%% fmap_(Op, ltlO_bound(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                                  call(Op, ltlO_bound(OL, OR), O).
%% fmap_(Op, ltlG_bound(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                                  call(Op, ltlG_bound(OL, OR), O).
%% fmap_(Op, ltlF_bound(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                                  call(Op, ltlF_bound(OL, OR), O).
%% fmap_(Op, ltlBefore_bound(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                                  call(Op, ltlBefore_bound(OL, OR), O).
%% fmap_(Op, ltlAfter_bound(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                                      call(Op, ltlAfter_bound(OL, OR), O).
%% fmap_(Op, ltlH(L), O) :- fmap_(Op, L, OL), call(Op, ltlH(OL), O).
%% fmap_(Op, ltlO(L), O) :- fmap_(Op, L, OL), call(Op, ltlO(OL), O).
%% fmap_(Op, ltlG(L), O) :- fmap_(Op, L, OL), call(Op, ltlG(OL), O).
%% fmap_(Op, ltlF(L), O) :- fmap_(Op, L, OL), call(Op, ltlF(OL), O).
%% %% fmap_(Op, ltlBefore(L), O) :- fmap_(Op, L, OL), call(Op, ltlBefore(OL), O).
%% %% fmap_(Op, ltlAfter(L), O) :- fmap_(Op, L, OL), call(Op, ltlAfter(OL), O).
%% fmap_(Op, ltlY(L), O) :- fmap_(Op, L, OL), call(Op, ltlY(OL), O).
%% fmap_(Op, ltlX(L), O) :- fmap_(Op, L, OL), call(Op, ltlX(OL), O).
%% fmap_(Op, ltlZ(L), O) :- fmap_(Op, L, OL), call(Op, ltlZ(OL), O).
%% fmap_(Op, ltlSI(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                             call(Op, ltlSI(OL, OR), O).
%% fmap_(Op, ltlS(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                            call(Op, ltlS(OL, OR), O).
%% fmap_(Op, ltlT(L), O) :- fmap_(Op, L, OL), call(Op, ltlT(OL), O).
%% fmap_(Op, ltlUI(L), O) :- fmap_(Op, L, OL), call(Op, ltlUI(OL), O).
%% fmap_(Op, ltlU(L), O) :- fmap_(Op, L, OL), call(Op, ltlU(OL), O).
%% fmap_(Op, ltlV(L), O) :- fmap_(Op, L, OL), call(Op, ltlV(OL), O).
%% fmap_(Op, not(E), O) :- fmap_(Op, E, I), call(Op, not(I), O).
%% fmap_(Op, eq(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                          call(Op, eq(OL, OR), O).
%% fmap_(Op, le(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                          call(Op, le(OL, OR), O).
%% fmap_(Op, ge(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                          call(Op, ge(OL, OR), O).
%% fmap_(Op, lt(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                          call(Op, lt(OL, OR), O).
%% fmap_(Op, gt(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                          call(Op, gt(OL, OR), O).
%% fmap_(Op, neq(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                           call(Op, neq(OL, OR), O).
%% fmap_(Op, next(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                            call(Op, next(OL, OR), O).
%% fmap_(Op, prev(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                            call(Op, prev(OL, OR), O).
%% fmap_(Op, boolcall(I, Args), O) :- call(Op, callid(I), callid(OI)),
%%                                   maplist(fmap_(Op), Args, OArgs),
%%                                   call(Op, boolcall(OI, OArgs), O).
%% fmap_(Op, and(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                           call(Op, and(OL, OR), O).
%% fmap_(Op, or(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                          call(Op, or(OL, OR), O).
%% fmap_(Op, xor(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                           call(Op, xor(OL, OR), O).
%% fmap_(Op, implies(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                               call(Op, implies(OL, OR), O).
%% fmap_(Op, equiv(L, R), O) :- fmap_(Op, L, OL), fmap_(Op, R, OR),
%%                             call(Op, equiv(OL, OR), O).
%% fmap_(Op, range2(A, B), O) :- fmap_(Op, A, OA), fmap_(Op, B, OB),
%%                              call(Op, range2(OA, OB), O).
%% fmap_(Op, salt_le(A), O) :- fmap_(Op, A, OA), call(Op, salt_le(OA), O).
%% fmap_(Op, salt_ge(A), O) :- fmap_(Op, A, OA), call(Op, salt_ge(OA), O).
%% fmap_(Op, salt_lt(A), O) :- fmap_(Op, A, OA), call(Op, salt_lt(OA), O).
%% fmap_(Op, salt_gt(A), O) :- fmap_(Op, A, OA), call(Op, salt_gt(OA), O).
%% fmap_(Op, salt_eq(A), O) :- fmap_(Op, A, OA), call(Op, salt_eq(OA), O).
%% fmap_(Op, salt_neq(A), O) :- fmap_(Op, A, OA), call(Op, salt_neq(OA), O).
%% fmap_(_, Elem, Elem) :-
%%     print_message(warning, no_fmap__for_elem(Elem)).

%% prolog:message(no_fmap__for_elem(Elem)) -->
%%     [ 'No fmap_ for ~w~n' - [Elem] ].

% ----------------------------------------------------------------------

optimize(add(val(X), val(Y)), val(V)) :- V is X + Y, !.
optimize(sub(val(X), val(Y)), val(V)) :- V is X - Y, !.
optimize(not(ltlY(true)), ltlZ(false)) :- !.
optimize(not(ltlZ(false)), ltlY(true)) :- !.
optimize(not(not(E)), E) :- !.
optimize(ltlS(L,and(L,R)), ltlSI(L,R)) :- !.
optimize(ltlS(L,and(R,L)), ltlSI(L,R)) :- !.
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
