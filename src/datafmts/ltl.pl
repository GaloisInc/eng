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
    define_language(LangDef, _).

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
% (causing →(⦂(if, bool),a) instead of ⦂(if, →(bool,a)) as expected).  Redeclare
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
        types: [ integer, bool ],
        atoms: [ ], % lit, num ],
        variable_ref: [ ident ],
        phrases:
        [ term(num ⦂ integer, num, emit_simple_term(num)),
          term(lit ⦂ bool, [true]>>word('TRUE'),
               [term(lit(true), bool),'TRUE']>>true), %emit_simple_term(lit)),
          term(lit ⦂ bool, [false]>>word('FALSE'),
               [_,'FALSE']>>true), %emit_simple_term(lit)),
          term(ident ⦂ a, word, emit_simple_term(ident)),
          term(ident ⦂ a, ltl:subst_ident, emit_simple_term(ident)),  % KWQ: different than ident??
          expop(not ⦂ bool → bool, [[]>>lexeme(chrs('!')), subexpr],
                [_,[A],T]>>fmt_str(T, '(! ~w)', [A])),
          expop(and ⦂ bool → bool → bool, infix(chrs('&')), emit_infix("&")),
          expop(or ⦂ bool → bool → bool, infix(chrs('|')), emit_infix("|")),
          expop(xor ⦂ bool → bool → bool, infix(tok(xor)), emit_infix("XOR")),
          % F - Finally (or Future or Eventually) [Timed]
          % arg eventually has to hold (somewhere on the subsequent path)
          expop(ltlF ⦂ bool → bool, [[]>>lexeme(word('F')), subexpr],
                [_,[A],T]>>fmt_str(T, '(F ~w)', [A])),
          % G - Globally [Timed]
          % arg has to hold on the ENTIRE subsequent path
          expop(ltlG ⦂ bool → bool, [[]>>lexeme(word('G')), subexpr],
                [_,[A],T]>>fmt_str(T, '(G ~w)', [A])),
          % H - Historically [Timed]
          expop(ltlH ⦂ bool → bool, [[]>>lexeme(word('H')), subexpr],
                [_,[A],T]>>fmt_str(T, '(H ~w)', [A])),
          % O - Once [Timed]
          expop(ltlO ⦂ bool → bool, [[]>>lexeme(word('O')), subexpr],
                [_,[A],T]>>fmt_str(T, '(O ~w)', [A])),
          % S - Since [Timed]  [[MTL]]
          % true at first argument and as long as second argument is true
          % X S Y   :: Out = X or (Y and (false -> pre Out)
          %
          %      A  B  C  D  E    F    G
          % X    ___1_____1__111__1____111__
          % Y    ______1__1__1____111__111__
          % Out  ___1_____1__1____111__111__
          expop(ltlS ⦂ bool → bool → bool, infix(chrs('S ')), emit_infix("S")),
          % SI - Since Inclusive [Timed]  [[MTL]]
          % true at first argument when and for as long as second argument is true
          % X SI Y  :: Out = Y and (X or (false -> pre Out)
          %
          %      A  B  C  D  E    F    G
          % X    ___1_____1__111__1____111__
          % Y    ______1__1__1____111__111__
          % Out  _________1__1____111__111__
          expop(ltlSI ⦂ bool → bool → bool, infix(chrs('SI ')), emit_infix("SI")),
          % T - Triggers  [[MITL]]
          expop(ltlT ⦂ bool → bool → bool, infix(word('T')), emit_infix("T")),
          % TT - Triggers Timed -- unsupported
          % U - Until [Timed]  [[MTL]]
          % first argument has to hold AT LEAST until second argument becomes
          % true, which must hold at the current or a future position
          expop(ltlU ⦂ bool → bool → bool, infix(word('U')), emit_infix("U")),
          % UI - Until Inclusive [Timed]  [[MTL]]
          expop(ltlUI ⦂ bool → bool → bool, infix(word('UI')), emit_infix("UI")),
          % V - Releases (converse of T), (aka R)
          % second argument has to be true until and including the point where
          % the first argument becomes true.  If the first argument never becomes
          % true, the second argument must remain true forever.
          expop(ltlV ⦂ bool → bool → bool, infix(word('V')), emit_infix("V")),
          % VT - Releases Timed -- unsupported
          expop(ltlO ⦂ bool → bool, [[]>>lexeme(word('O')), subexpr],
                [_,[A],T]>>fmt_str(T, '(O ~w)', [A])),
          % X - Next
          % arg must hold (be true) at the next state
          expop(ltlX ⦂ bool → bool, [[]>>lexeme(word('X')), subexpr],
                [_,[A],T]>>fmt_str(T, '(X ~w)', [A])),
          % Y - PrevFalse
          expop(ltlY ⦂ bool → bool, [[]>>lexeme(word('Y')), subexpr],
                [_,[A],T]>>fmt_str(T, '(Y ~w)', [A])),
          % Z - PrevTrue
          expop(ltlZ ⦂ bool → bool, [[]>>lexeme(word('Z')), subexpr],
                [_,[A],T]>>fmt_str(T, '(Z ~w)', [A])),
          % TODO: allowing this creates a parse ambiguity: is FALSE the term(lit(false), bool), or is is an ltlF_bound(term(ident("ALSE"), bool), _) ??
          %% expop(ltlF_bound ⦂ range → bool → bool,
          %%       [[]>>lexeme(chrs('F')), subexpr, subexpr],
          %%       [_,[R,A],T]>>fmt_str(T, '(F~w ~w)', [R,A])),
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
          expop(before_bound ⦂ integer → bool → bool,
                [[]>>lexeme(chrs('<|')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str('(<| ~w ~w)', [R, A])),
          expop(before ⦂ bool → bool,
                [[]>>lexeme(chrs('<|')), subexpr],
                [_,[A],T]>>fmt_str('(<| ~w)', [A])),
          expop(after_bound ⦂ integer → bool → bool,
                [[]>>lexeme(chrs('|>')), subexpr, subexpr],
                [_,[R,A],T]>>fmt_str('(|> ~w ~w)', [R, A])),
          expop(after ⦂ bool → bool,
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
          expop(implies ⦂ bool → bool → bool, infix(chrs('->')), emit_infix("->")),
          expop(eq ⦂ a → a → bool, infix(chrs('=')), emit_infix("=")),
          expop(equiv ⦂ a → a → bool, infix(chrs('<=>')), emit_infix("<=>")),
          expop(equiv ⦂ a → a → bool, infix(chrs('<->')), emit_infix("<=>")),
          expop(neq ⦂ a → a → bool, infix(chrs('!=')), emit_infix("!=")),
          expop(gteq ⦂ integer → integer → bool, infix(chrs('>=')), emit_infix(">=")),
          expop(lteq ⦂ integer → integer → bool, infix(chrs('<=')), emit_infix("<=")),
          expop(gt ⦂ integer → integer → bool, infix(chrs('>')), emit_infix(">")),
          expop(lt ⦂ integer → integer → bool, infix(chrs('<')), emit_infix("<")),
          expop(neg ⦂ integer → integer, [[]>>lexeme(chrs('-')), subexpr],
                [_,[A],T]>>fmt_str(T, '(- ~w)', [A])),
          expop(add ⦂ integer → integer → integer, infix(chrs('+')), emit_infix("+")),
          expop(sub ⦂ integer → integer → integer, infix(chrs('-')), emit_infix("-")),
          expop(mul ⦂ integer → integer → integer, infix(chrs('*')), emit_infix("*")),
          expop(divd ⦂ integer → integer → integer, infix(chrs('/')), emit_infix("/")),
          expop(expo ⦂ integer → integer → integer, infix(chrs('^')), emit_infix("^"))
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
optimize(op(not(op(not(E), bool)), bool), E) :- !.
optimize(op(or(E, E), bool), E) :- !.
optimize(op(and(E, E), bool), E) :- !.
% !(!p & !q) --> p | q
optimize(op(not(op(and(op(not(P), bool), op(not(Q), bool)), bool), bool)),
         op(or(P, Q), bool)) :- !.
% (p & q) | (p & r) --> p & (q | r)  [and variations]
optimize(op(or(op(and(P, Q), bool), op(and(P, R), bool)), bool),
         op(and(P, op(or(Q, R), bool)), bool)) :- !.
optimize(op(or(op(and(Q, P), bool), op(and(P, R), bool)), bool),
         op(and(P, op(or(Q, R), bool)), bool)) :- !.
optimize(op(or(op(and(P, Q), bool), op(and(R, P), bool)), bool),
         op(and(P, op(or(Q, R), bool)), bool)) :- !.
optimize(op(or(op(and(Q, P), bool), op(and(R, P), bool)), bool),
         op(and(P, op(or(Q, R), bool)), bool)) :- !.
% !p | (p & q) --> !p | q  [and variations]
optimize(op(or(op(not(P), bool), op(and(P, Q), bool)), bool),
         op(or(op(not(P), bool), Q), bool)) :- !.
optimize(op(or(op(not(P), bool), op(and(Q, P), bool)), bool),
         op(or(op(not(P), bool), Q), bool)) :- !.
% !p & !(q | p) --> !p & !q [and variations]
optimize(op(and(op(not(P), bool), op(not(op(or(Q, P), bool)), bool)), bool),
         op(and(op(not(P), bool), op(not(Q), bool)), bool)) :- !.
optimize(op(and(op(not(P), bool), op(not(op(or(P, Q), bool)), bool)), bool),
         op(and(op(not(P), bool), op(not(Q), bool)), bool)) :- !.
% !p & !(q & p) --> !p [and variations]
optimize(op(and(op(not(P), bool), op(not(op(and(_, P), bool)), bool)), bool),
         op(not(P), bool)) :- !.
optimize(op(and(op(not(P), bool), op(not(op(and(P, _), bool)), bool)), bool),
         op(not(P), bool)) :- !.
% p & !(!p & q) --> p  [and variations]
optimize(op(and(P, op(not(op(and(op(not(P), bool), _), bool)), bool)), bool),
         P) :- !.
optimize(op(and(P, op(not(op(and(_, op(not(P), bool)), bool)), bool)), bool),
         P) :- !.
% !(p & q) & !(r | q) --> !q & !r [ and variations ]
optimize(op(and(op(not(op(and(_, Q), bool)), bool), op(not(op(or(R, Q), bool)), bool)), bool),
         op(and(op(not(Q), bool), op(not(R), bool)), bool)) :- !.
optimize(op(and(op(not(op(and(Q, _), bool)), bool), op(not(op(or(R, Q), bool)), bool)), bool),
         op(and(op(not(Q), bool), op(not(R), bool)), bool)) :- !.
optimize(op(and(op(not(op(and(_, Q), bool)), bool), op(not(op(or(Q, R), bool)), bool)), bool),
         op(and(op(not(Q), bool), op(not(R), bool)), bool)) :- !.
optimize(op(and(op(not(op(and(Q, _), bool)), bool), op(not(op(or(Q, R), bool)), bool)), bool),
         op(and(op(not(Q), bool), op(not(R), bool)), bool)) :- !.
optimize(op(and(op(not(op(or(R, Q), bool)), bool), op(not(op(and(_, Q), bool)), bool)), bool),
         op(and(op(not(Q), bool), op(not(R), bool)), bool)) :- !.
optimize(op(and(op(not(op(or(Q, R), bool)), bool), op(not(op(and(_, Q), bool)), bool)), bool),
         op(and(op(not(Q), bool), op(not(R), bool)), bool)) :- !.
optimize(op(and(op(not(op(or(R, Q), bool)), bool), op(not(op(and(Q, _), bool)), bool)), bool),
         op(and(op(not(Q), bool), op(not(R), bool)), bool)) :- !.
optimize(op(and(op(not(op(or(Q, R), bool)), bool), op(not(op(and(Q, _), bool)), bool)), bool),
         op(and(op(not(Q), bool), op(not(R), bool)), bool)) :- !.

optimize(op(not(term(lit(false), bool)), bool), term(lit(true), bool)) :- !.
optimize(op(not(term(lit(true), bool)), bool), term(lit(false), bool)) :- !.

optimize(op(or(term(lit(false), bool), E), bool), E) :- !.
optimize(op(or(E, term(lit(false), bool)), bool), E) :- !.
optimize(op(or(term(lit(true), bool), _), bool), term(lit(true), bool)) :- !.
optimize(op(or(_, term(lit(true), bool)), bool), term(lit(true), bool)) :- !.
optimize(op(and(term(lit(true), bool), E), bool), E) :- !.
optimize(op(and(E, term(lit(true), bool)), bool), E) :- !.
optimize(op(and(term(lit(false), bool), _), bool), term(lit(false), bool)) :- !.
optimize(op(and(_, term(lit(false), bool)), bool), term(lit(false), bool)) :- !.

optimize(op(implies(term(lit(true), bool), E), bool), E) :- !.
optimize(op(implies(term(lit(false), bool), _), bool), term(lit(true), bool)) :- !.

% These optimizations parallel those in fret-electron/support/xform.js
% pastTimeSimplications
optimize(op(not(op(ltlY(term(lit(true), bool)), bool)), bool),
         op(ltlZ(term(lit(false), bool)), bool)) :- !.
optimize(op(not(op(ltlZ(term(lit(false), bool)), bool)), bool),
         op(ltlY(term(lit(true), bool)), bool)) :- !.
optimize(op(ltlO(term(lit(false), bool)), bool), term(lit(false), bool)) :- !.
optimize(op(ltlO(term(lit(true), bool)), bool), term(lit(true), bool)) :- !.
% O(Z _) --> true
optimize(op(ltlO(op(ltlZ(_), bool)), bool), term(lit(true), bool)) :- !.
% H lit -> lit
optimize(op(ltlH(term(lit(B), bool)), bool), term(lit(B), bool)) :- !.
% H (Z FALSE) -> Z FALSE
optimize(op(ltlH(op(ltlZ(term(lit(false), bool)), bool)), bool),
         op(ltlZ(term(lit(false), bool)), bool)) :- !.
% H (p & q) --> H p & H q
optimize(op(ltlH(op(and(P, Q), bool)), bool),
         op(and(op(ltlH(P), bool), op(ltlH(Q), bool)), bool)) :- !.
% H (O p) --> H ((Y TRUE) | p)
optimize(op(ltlH(op(ltlO(P), bool)), bool),
         op(ltlH(op(or(op(ltlY(term(lit(true), bool)), bool), P), bool)), bool)) :- !.
% H (H[0, r] p) --> H p
optimize(op(ltlH(op(ltlH_bound(Range, P), bool)), bool), op(ltlH(P), bool)) :-
    member(Range, [ op(range_min_max(term(num(0), integer),
                                     term(_, integer)), range),
                    op(range_max(_), range),
                    op(range_max_incl(_), range)
                  ]),
    !.
% H (Y TRUE) --> FALSE
optimize(op(ltlH(op(ltlY(term(lit(true), bool)), bool)), bool),
         term(lit(false), bool)) :- !.
% !(H (!p)) --> O p
optimize(op(not(op(ltlH(op(not(P), bool)), bool)), bool),
         op(ltlO(P), bool)) :- !.
% !(O (!p)) --> H p
optimize(op(not(op(ltlO(op(not(P), bool)), bool)), bool),
         op(ltlH(P), bool)) :- !.
% true S p --> O p
optimize(op(ltlS(term(lit(true), bool), P), bool), op(ltlO(P), bool)) :- !.
% p S (p & Z false) --> H p   [and variations]
optimize(op(ltlS(P, op(and(P, ZF), bool)), bool), op(ltlH(P), bool)) :-
    ZF = op(ltlZ(term(lit(false), bool)), bool), !.
optimize(op(ltlS(P, op(and(ZF, P), bool)), bool), op(ltlH(P), bool)) :-
    ZF = op(ltlZ(term(lit(false), bool)), bool), !.
% p S p --> p
optimize(op(ltlS(P, P), bool), P) :- !.
% ((Y true) & p) S q --> p S q   [and variations]
optimize(op(ltlS(op(and(op(ltlY(term(lit(true), bool)), bool), P), bool), Q), bool),
         op(ltlS(P, Q), bool)) :- !.
optimize(op(ltlS(op(and(op(P, ltlY(term(lit(true), bool)), bool)), bool), Q), bool),
         op(ltlS(P, Q), bool)) :- !.
% (Y true) S q --> O q
optimize(op(ltlS(op(ltlY(term(lit(true), bool)), bool), Q), bool),
         op(ltlO(Q), bool)) :- !.
% (Y true) & (Y p) --> Y p   [and variations]
optimize(op(and(op(ltlY(term(lit(true), bool)), bool), op(ltlY(P), bool)), bool),
         op(ltlY(P), bool)) :- !.
optimize(op(and(op(ltlY(P), bool), op(ltlY(term(lit(true), bool)), bool)), bool),
         op(ltlY(P), bool)) :- !.
% (Z false) | Y p -> Z p   [and variations]
optimize(op(or(op(ltlZ(term(lit(false), bool)), bool), op(ltlY(P), bool)), bool),
         op(ltlZ(P), bool)) :- !.
optimize(op(or(op(ltlY(P), bool), op(ltlZ(term(lit(false), bool)), bool)), bool),
         op(ltlZ(P), bool)) :- !.


% These optimizations parallel those in
% fret-electron/support/LTLParser/LTLASTSemantics.js introduce_SinceInclusive.
% These are verified by tests/S_SI_xform.lus.
optimize(op(ltlS(L,op(and(L,R), bool)), bool), op(ltlSI(L,R), bool)) :- !.
optimize(op(ltlS(L,op(and(R,L), bool)), bool), op(ltlSI(L,R), bool)) :- !.

optimize(X, X).


% ----------------------------------------------------------------------
