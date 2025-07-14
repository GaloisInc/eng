:- encoding(utf8).
% Parses the LTL input text into an AST.

:- module(ltl, [ define_ltl_language/0,
                 parse_ltl/2,
                 emit_ltl/2,
                 ltl_langdef/1
               ]).

:- use_module('../englib').
:- use_module('../exprlang').

define_ltl_language() :-
    ltl_langdef(LangDef),
    ensure_language_defined(LangDef).

parse_ltl(Inp, AST) :-
    ltl_langdef(LangDef),
    get_dict(language, LangDef, Language),
    parse_expr(Language, Inp, RawAST),
    (fmap_abt(Language, ltl:optimize, RawAST, AST) ; AST = RawAST).

emit_ltl(AST, Text) :-
    ltl_langdef(LangDef),
    get_dict(language, LangDef, Language),
    emit_expr(Language, AST, Text).

% These ops are exported from exprlang, but apparently their precedence is lost
% (causing →(⦂(if, boolean),a) instead of ⦂(if, →(boolean,a)) as expected).  Redeclare
% their precedence here.
:- op(760, yfx, ⦂).
:- op(750, xfy, →).

% Special parser for a $ident$ which represents a substitution identifier.
subst_ident(I) --> chrs('$'), word(W), chrs('$'), {format(atom(I), '$~w$', W)}.

% LTL (Linear Temporal Logic) and MTL (Metric Temporal Logic) and MITL (Metric
% Interval Temporal Logic) expressions.
%
% Matches fret-electron/sjpport/LTLParser/LTLASTSemantics.js
%
ltl_langdef(
    langdef{
        language: ltl,
        types: [ integer, boolean ],
        atoms: [ ], % lit, num ],
        variable_ref: [ ident ],
        phrases:
        [ term(num ⦂ integer, num, emit_simple_term(num)),
          term(lit ⦂ boolean, [true]>>word('TRUE'),
               [term(lit(true), boolean),'TRUE']>>true), %emit_simple_term(lit)),
          term(lit ⦂ boolean, [false]>>word('FALSE'),
               [_,'FALSE']>>true), %emit_simple_term(lit)),
          term(ident ⦂ a, word, emit_simple_term(ident)),
          term(ident ⦂ a, ltl:subst_ident, emit_simple_term(ident)),  % KWQ: different than ident??
          expop(not ⦂ boolean → boolean, [[]>>lexeme(chrs('!')), subexpr],
                [_,[A],T]>>fmt_str(T, '(! ~w)', [A])),
          expop(and ⦂ boolean → boolean → boolean, infix(chrs('&')), emit_infix("&")),
          expop(or ⦂ boolean → boolean → boolean, infix(chrs('|')), emit_infix("|")),
          expop(xor ⦂ boolean → boolean → boolean, infix(tok(xor)), emit_infix("XOR")),
          % F - Finally (or Future or Eventually) [Timed]
          % arg eventually has to hold (somewhere on the subsequent path)
          expop(ltlF ⦂ boolean → boolean, [[]>>lexeme(word('F')), subexpr],
                [_,[A],T]>>fmt_str(T, '(F ~w)', [A])),
          % G - Globally [Timed]
          % arg has to hold on the ENTIRE subsequent path
          expop(ltlG ⦂ boolean → boolean, [[]>>lexeme(word('G')), subexpr],
                [_,[A],T]>>fmt_str(T, '(G ~w)', [A])),
          % H - Historically [Timed]
          expop(ltlH ⦂ boolean → boolean, [[]>>lexeme(word('H')), subexpr],
                [_,[A],T]>>fmt_str(T, '(H ~w)', [A])),
          % O - Once [Timed]
          expop(ltlO ⦂ boolean → boolean, [[]>>lexeme(word('O')), subexpr],
                [_,[A],T]>>fmt_str(T, '(O ~w)', [A])),
          % S - Since [Timed]  [[MTL]]
          % true at first argument and as long as second argument is true
          % X S Y   :: Out = X or (Y and (false -> pre Out)
          %
          %      A  B  C  D  E    F    G
          % X    ___1_____1__111__1____111__
          % Y    ______1__1__1____111__111__
          % Out  ___1_____1__1____111__111__
          expop(ltlS ⦂ boolean → boolean → boolean, infix(chrs('S ')), emit_infix("S")),
          % SI - Since Inclusive [Timed]  [[MTL]]
          % true at first argument when and for as long as second argument is true
          % X SI Y  :: Out = Y and (X or (false -> pre Out)
          %
          %      A  B  C  D  E    F    G
          % X    ___1_____1__111__1____111__
          % Y    ______1__1__1____111__111__
          % Out  _________1__1____111__111__
          expop(ltlSI ⦂ boolean → boolean → boolean, infix(chrs('SI ')), emit_infix("SI")),
          % T - Triggers  [[MITL]]
          expop(ltlT ⦂ boolean → boolean → boolean, infix(word('T')), emit_infix("T")),
          % TT - Triggers Timed -- unsupported
          % U - Until [Timed]  [[MTL]]
          % first argument has to hold AT LEAST until second argument becomes
          % true, which must hold at the current or a future position
          expop(ltlU ⦂ boolean → boolean → boolean, infix(word('U')), emit_infix("U")),
          % UI - Until Inclusive [Timed]  [[MTL]]
          expop(ltlUI ⦂ boolean → boolean → boolean, infix(word('UI')), emit_infix("UI")),
          % V - Releases (converse of T), (aka R)
          % second argument has to be true until and including the point where
          % the first argument becomes true.  If the first argument never becomes
          % true, the second argument must remain true forever.
          expop(ltlV ⦂ boolean → boolean → boolean, infix(word('V')), emit_infix("V")),
          % VT - Releases Timed -- unsupported
          expop(ltlO ⦂ boolean → boolean, [[]>>lexeme(word('O')), subexpr],
                [_,[A],T]>>fmt_str(T, '(O ~w)', [A])),
          % X - Next
          % arg must hold (be true) at the next state
          expop(ltlX ⦂ boolean → boolean, [[]>>lexeme(word('X')), subexpr],
                [_,[A],T]>>fmt_str(T, '(X ~w)', [A])),
          % Y - PrevFalse
          expop(ltlY ⦂ boolean → boolean, [[]>>lexeme(word('Y')), subexpr],
                [_,[A],T]>>fmt_str(T, '(Y ~w)', [A])),
          % Z - PrevTrue
          expop(ltlZ ⦂ boolean → boolean, [[]>>lexeme(word('Z')), subexpr],
                [_,[A],T]>>fmt_str(T, '(Z ~w)', [A])),
          expop(ltlF_bound ⦂ range → boolean → boolean,
                [[]>>lexeme(chrs('F')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str(T, '(F~w ~w)', [R,A])),
          expop(ltlG_bound ⦂ range → boolean → boolean,
                [[]>>lexeme(chrs('G')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str(T, '(G~w ~w)', [R,A])),
          expop(ltlH_bound ⦂ range → boolean → boolean,
                [[]>>lexeme(chrs('H')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str(T, '(H~w ~w)', [R,A])),
          expop(ltlO_bound ⦂ range → boolean → boolean,
                [[]>>lexeme(chrs('O')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str(T, '(O~w ~w)', [R,A])),
          % TODO: these are LL non-productive and therefore cause infinite recursion.  But are they even valid?
          %% expop(ltlS_bound ⦂ boolean → range → boolean → boolean,
          %%       [subexpr, []>>lexeme(chrs('SI')), subexpr, subexpr],
          %%       [_,[B,R,A],T]>>fmt_str(T, '(~w SI~w ~w)', [B,R,A])),
          %% expop(ltlU_bound ⦂ boolean → range → boolean → boolean,
          %%       [subexpr, []>>lexeme(chrs('UI')), subexpr, subexpr],
          %%       [_,[B,R,A],T]>>fmt_str(T, '(~w UI~w ~w)', [B,R,A])),
          expop(before_bound ⦂ integer → boolean → boolean,
                [[]>>lexeme(chrs('<|')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str('(<| ~w ~w)', [R, A])),
          expop(before ⦂ boolean → boolean,
                [[]>>lexeme(chrs('<|')), subexpr],
                [_,[A],T]>>fmt_str('(<| ~w)', [A])),
          expop(after_bound ⦂ integer → boolean → boolean,
                [[]>>lexeme(chrs('|>')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str('(|> ~w ~w)', [R, A])),
          expop(after ⦂ boolean → boolean,
                [[]>>lexeme(chrs('|>')), subexpr],
                [_,[A],T]>>fmt_str('(|> ~w)', [A])),
          %% vv --- begin salt bounds specification
          expop(range_exact ⦂ integer → range,
                [[]>>lexeme(chrs('[=')), subexpr, lexeme(chrs(']'))],
                [_,[R],T]>>fmt_str(T, '[=~w]', [R])),
          expop(range_max_incl ⦂ integer → range,
                [[]>>lexeme(chrs('[<=')), subexpr, lexeme(chrs(']'))],
                [_,[R],T]>>fmt_str(T, '[<=~w]', [R])),
          expop(range_max ⦂ integer → range,
                [[]>>lexeme(chrs('[<')), subexpr, lexeme(chrs(']'))],
                [_,[R],T]>>fmt_str(T, '[<~w]', [R])),
          expop(range_min_incl ⦂ integer → range,
                [[]>>lexeme(chrs('[>=')), subexpr, lexeme(chrs(']'))],
                [_,[R],T]>>fmt_str(T, '[>=~w]', [R])),
          expop(range_min ⦂ integer → range,
                [[]>>lexeme(chrs('[>')), subexpr, lexeme(chrs(']'))],
                [_,[R],T]>>fmt_str(T, '[>~w]', [R])),
          %% TODO: [!=   ... not sure now this is emitted in Lustre.  Needed?
          %% ^^ --- end salt bounds specification
          expop(range_min_max ⦂ integer → integer → range,
                [[]>>lexeme(chrs('[')), subexpr,
                 lexeme(chrs(',')), subexpr, lexeme(chrs(']'))],
                [_,[L,R],T]>>fmt_str(T, '[~w, ~w]', [L, R])),
          expop(implies ⦂ boolean → boolean → boolean, infix(chrs('->')), emit_infix("->")),
          expop(eq ⦂ a → a → boolean, infix(chrs('=')), emit_infix("=")),
          expop(equiv ⦂ a → a → boolean, infix(chrs('<=>')), emit_infix("<=>")),
          expop(equiv ⦂ a → a → boolean, infix(chrs('<->')), emit_infix("<=>")),
          expop(neq ⦂ a → a → boolean, infix(chrs('!=')), emit_infix("!=")),
          expop(gteq ⦂ integer → integer → boolean, infix(chrs('>=')), emit_infix(">=")),
          expop(lteq ⦂ integer → integer → boolean, infix(chrs('<=')), emit_infix("<=")),
          expop(gt ⦂ integer → integer → boolean, infix(chrs('>')), emit_infix(">")),
          expop(lt ⦂ integer → integer → boolean, infix(chrs('<')), emit_infix("<")),
          expop(neg ⦂ integer → integer, [[]>>lexeme(chrs('-')), subexpr],
                [_,[A],T]>>fmt_str(T, '(- ~w)', [A])),
          expop(add ⦂ integer → integer → integer, infix(chrs('+')), emit_infix("+")),
          expop(sub ⦂ integer → integer → integer, infix(chrs('-')), emit_infix("-")),
          expop(mul ⦂ integer → integer → integer, infix(chrs('*')), emit_infix("*")),
          expop(divd ⦂ integer → integer → integer, infix(chrs('/')), emit_infix("/")),
          expop(expo ⦂ integer → integer → integer, infix(chrs('^')), emit_infix("^")),
          expop(fby ⦂ a → a → a, infix(chrs('->')), emit_infix("->"))
          %% TODO: at the next occurrence of BOOL
          %% TODO: at the previous occurrence of BOOL
        ]}).


% ----------------------------------------------------------------------

% Optimizations are initiated from fret-electron/app/parser/CacheSemantics.js

% These optimizations parallel those in fret-electron/;support/xform.js
optimize(op(add(term(num(X), integer), term(num(Y), integer)), integer), term(num(V), integer)) :- V is X + Y, writeln(opt_add_consts), !.
optimize(op(sub(term(num(X), integer), term(num(Y), integer)), integer), term(num(V), integer)) :- V is X - Y, writeln(opt_sub_consts), !.

% These optimizations parallel those in fret-electron/;support/xform.js
% booleanSimplifications
optimize(op(not(op(not(E), boolean)), boolean), E).
optimize(op(or(E, E), boolean), E).
optimize(op(and(E, E), boolean), E).
% !(!p & !q) --> p | q
optimize(op(not(op(and(op(not(P), boolean),
                       op(not(Q), boolean)), boolean), boolean)),
         op(or(P, Q), boolean)).
% (p & q) | (p & r) --> p & (q | r)  [and variations]
optimize(op(or(op(and(P, Q), boolean), op(and(P, R), boolean)), boolean),
         op(and(P, op(or(Q, R), boolean)), boolean)).
optimize(op(or(op(and(Q, P), boolean), op(and(P, R), boolean)), boolean),
         op(and(P, op(or(Q, R), boolean)), boolean)).
optimize(op(or(op(and(P, Q), boolean), op(and(R, P), boolean)), boolean),
         op(and(P, op(or(Q, R), boolean)), boolean)).
optimize(op(or(op(and(Q, P), boolean), op(and(R, P), boolean)), boolean),
         op(and(P, op(or(Q, R), boolean)), boolean)).
% !p | (p & q) --> !p | q  [and variations]
optimize(op(or(op(not(P), boolean), op(and(P, Q), boolean)), boolean),
         op(or(op(not(P), boolean), Q), boolean)).
optimize(op(or(op(not(P), boolean), op(and(Q, P), boolean)), boolean),
         op(or(op(not(P), boolean), Q), boolean)).
% !p & !(q | p) --> !p & !q [and variations]
optimize(op(and(op(not(P), boolean),
                op(not(op(or(Q, P), boolean)), boolean)), boolean),
         op(and(op(not(P), boolean), op(not(Q), boolean)), boolean)).
optimize(op(and(op(not(P), boolean),
                op(not(op(or(P, Q), boolean)), boolean)), boolean),
         op(and(op(not(P), boolean), op(not(Q), boolean)), boolean)).
% !p & !(q & p) --> !p [and variations]
optimize(op(and(op(not(P), boolean),
                op(not(op(and(_, P), boolean)), boolean)), boolean),
         op(not(P), boolean)).
optimize(op(and(op(not(P), boolean),
                op(not(op(and(P, _), boolean)), boolean)), boolean),
         op(not(P), boolean)).
% p & !(!p & q) --> p  [and variations]
optimize(op(and(P,
                op(not(op(and(op(not(P), boolean), _), boolean)), boolean)), boolean),
         P).
optimize(op(and(P,
                op(not(op(and(_, op(not(P), boolean)), boolean)), boolean)), boolean),
         P).
% !(p & q) & !(r | q) --> !q & !r [ and variations ]
optimize(op(and(op(not(op(and(_, Q), boolean)), boolean),
                op(not(op(or(R, Q), boolean)), boolean)), boolean),
         op(and(op(not(Q), boolean), op(not(R), boolean)), boolean)).
optimize(op(and(op(not(op(and(Q, _), boolean)), boolean),
                op(not(op(or(R, Q), boolean)), boolean)), boolean),
         op(and(op(not(Q), boolean), op(not(R), boolean)), boolean)).
optimize(op(and(op(not(op(and(_, Q), boolean)), boolean),
                op(not(op(or(Q, R), boolean)), boolean)), boolean),
         op(and(op(not(Q), boolean), op(not(R), boolean)), boolean)).
optimize(op(and(op(not(op(and(Q, _), boolean)), boolean),
                op(not(op(or(Q, R), boolean)), boolean)), boolean),
         op(and(op(not(Q), boolean), op(not(R), boolean)), boolean)).
optimize(op(and(op(not(op(or(R, Q), boolean)), boolean),
                op(not(op(and(_, Q), boolean)), boolean)), boolean),
         op(and(op(not(Q), boolean), op(not(R), boolean)), boolean)).
optimize(op(and(op(not(op(or(Q, R), boolean)), boolean),
                op(not(op(and(_, Q), boolean)), boolean)), boolean),
         op(and(op(not(Q), boolean), op(not(R), boolean)), boolean)).
optimize(op(and(op(not(op(or(R, Q), boolean)), boolean),
                op(not(op(and(Q, _), boolean)), boolean)), boolean),
         op(and(op(not(Q), boolean), op(not(R), boolean)), boolean)).
optimize(op(and(op(not(op(or(Q, R), boolean)), boolean),
                op(not(op(and(Q, _), boolean)), boolean)), boolean),
         op(and(op(not(Q), boolean), op(not(R), boolean)), boolean)).

optimize(op(not(term(lit(false), boolean)), boolean), term(lit(true), boolean)).
optimize(op(not(term(lit(true), boolean)), boolean), term(lit(false), boolean)).

optimize(op(or(term(lit(false), boolean), E), boolean), E).
optimize(op(or(E, term(lit(false), boolean)), boolean), E).
optimize(op(or(term(lit(true), boolean), _), boolean), term(lit(true), boolean)).
optimize(op(or(_, term(lit(true), boolean)), boolean), term(lit(true), boolean)).
optimize(op(and(term(lit(true), boolean), E), boolean), E).
optimize(op(and(E, term(lit(true), boolean)), boolean), E).
optimize(op(and(term(lit(false), boolean), _), boolean),
         term(lit(false), boolean)).
optimize(op(and(_, term(lit(false), boolean)), boolean),
         term(lit(false), boolean)).

optimize(op(implies(term(lit(true), boolean), E), boolean), E).
optimize(op(implies(term(lit(false), boolean), _), boolean),
         term(lit(true), boolean)).

% These optimizations parallel those in fret-electron/support/xform.js
% pastTimeSimplications
optimize(op(not(op(ltlY(term(lit(true), boolean)), boolean)), boolean),
         op(ltlZ(term(lit(false), boolean)), boolean)).
optimize(op(not(op(ltlZ(term(lit(false), boolean)), boolean)), boolean),
         op(ltlY(term(lit(true), boolean)), boolean)).
optimize(op(ltlO(term(lit(false), boolean)), boolean), term(lit(false), boolean)).
optimize(op(ltlO(term(lit(true), boolean)), boolean), term(lit(true), boolean)).
% O(Z _) --> true
optimize(op(ltlO(op(ltlZ(_), boolean)), boolean), term(lit(true), boolean)).
% H lit -> lit
optimize(op(ltlH(term(lit(B), boolean)), boolean), term(lit(B), boolean)).
% H (Z FALSE) -> Z FALSE
optimize(op(ltlH(op(ltlZ(term(lit(false), boolean)), boolean)), boolean),
         op(ltlZ(term(lit(false), boolean)), boolean)).
% H (p & q) --> H p & H q
optimize(op(ltlH(op(and(P, Q), boolean)), boolean),
         op(and(op(ltlH(P), boolean), op(ltlH(Q), boolean)), boolean)).
% H (O p) --> H ((Y TRUE) | p)
optimize(op(ltlH(op(ltlO(P), boolean)), boolean),
         op(ltlH(op(or(op(ltlY(term(lit(true), boolean)), boolean),
                       P), boolean)), boolean)).
% H (H[0, r] p) --> H p
optimize(op(ltlH(op(ltlH_bound(Range, P), boolean)), boolean),
         op(ltlH(P), boolean)) :-
    member(Range, [ op(range_min_max(term(num(0), integer),
                                     term(_, integer)), range),
                    op(range_max(_), range),
                    op(range_max_incl(_), range)
                  ]),
    !.
% H (Y TRUE) --> FALSE
optimize(op(ltlH(op(ltlY(term(lit(true), boolean)), boolean)), boolean),
         term(lit(false), boolean)).
% !(H (!p)) --> O p
optimize(op(not(op(ltlH(op(not(P), boolean)), boolean)), boolean),
         op(ltlO(P), boolean)).
% !(O (!p)) --> H p
optimize(op(not(op(ltlO(op(not(P), boolean)), boolean)), boolean),
         op(ltlH(P), boolean)).
% true S p --> O p
optimize(op(ltlS(term(lit(true), boolean), P), boolean),
         op(ltlO(P), boolean)).
% p S (p & Z false) --> H p   [and variations]
optimize(op(ltlS(P, op(and(P, ZF), boolean)), boolean), op(ltlH(P), boolean)) :-
    ZF = op(ltlZ(term(lit(false), boolean)), boolean), !.
optimize(op(ltlS(P, op(and(ZF, P), boolean)), boolean), op(ltlH(P), boolean)) :-
    ZF = op(ltlZ(term(lit(false), boolean)), boolean), !.
% p S p --> p
optimize(op(ltlS(P, P), boolean), P).
% ((Y true) & p) S q --> p S q   [and variations]
optimize(op(ltlS(op(and(op(ltlY(term(lit(true), boolean)), boolean),
                        P), boolean),
                 Q), boolean),
         op(ltlS(P, Q), boolean)).
optimize(op(ltlS(op(and(P,
                        op(ltlY(term(lit(true), boolean)), boolean)), boolean),
                 Q), boolean),
         op(ltlS(P, Q), boolean)).
% (Y true) S q --> O q
optimize(op(ltlS(op(ltlY(term(lit(true), boolean)), boolean), Q), boolean),
         op(ltlO(Q), boolean)).
% (Y true) & (Y p) --> Y p   [and variations]
optimize(op(and(op(ltlY(term(lit(true), boolean)), boolean),
                op(ltlY(P), boolean)), boolean),
         op(ltlY(P), boolean)).
optimize(op(and(op(ltlY(P), boolean),
                op(ltlY(term(lit(true), boolean)), boolean)), boolean),
         op(ltlY(P), boolean)).
% (Z false) | Y p -> Z p   [and variations]
optimize(op(or(op(ltlZ(term(lit(false), boolean)), boolean),
               op(ltlY(P), boolean)), boolean),
         op(ltlZ(P), boolean)).
optimize(op(or(op(ltlY(P), boolean),
               op(ltlZ(term(lit(false), boolean)), boolean)), boolean),
         op(ltlZ(P), boolean)).


% These optimizations parallel those in
% fret-electron/support/LTLParser/LTLASTSemantics.js introduce_SinceInclusive.
% These are verified by tests/S_SI_xform.lus.
optimize(op(ltlS(L,op(and(L,R), boolean)), boolean), op(ltlSI(L,R), boolean)).
optimize(op(ltlS(L,op(and(R,L), boolean)), boolean), op(ltlSI(L,R), boolean)).

optimize(term("LTL", bool), op(fby(term(lit(true), boolean),
                                   term(lit(false), boolean)), boolean)) :- !.

optimize(X, X).


% ----------------------------------------------------------------------
