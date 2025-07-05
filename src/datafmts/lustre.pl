% Parses and emits Lustre expressions via an AST.  Note that this expects to
% process the same terms and AST emitted by the ltl parser.

:- encoding(utf8).
:- module(lustre, [ define_lustre_language/0,
                    parse_lustre/2, emit_lustre/2, emit_CoCoSpec/2 ]).

:- use_module('../englib').
:- use_module('../exprlang').

define_lustre_language :-
    lustre_langdef(LangDef),
    ensure_language_defined(LangDef).

parse_lustre(Inp, AST) :-
    lustre_langdef(LangDef),
    get_dict(language, LangDef, Language),
    parse_expr(Language, Inp, AST).

emit_CoCoSpec(AST, Text) :- emit_lustre(AST, Text).

emit_lustre(AST, Text) :-
    lustre_langdef(LangDef),
    get_dict(language, LangDef, Language),
    emit_expr(Language, AST, Text).

% These ops are exported from exprlang, but apparently their precedence is lost
% (causing →(⦂(if, boolean),a) instead of ⦂(if, →(boolean,a)) as expected).  Redeclare
% their precedence here.
:- op(760, yfx, ⦂).
:- op(750, xfy, →).

lustre_langdef(
    langdef{
        language: lustre,
        types: [ integer, boolean ],
        atoms: [ ], % lit, num ],
        variable_ref: [ ident ],
        phrases:
        [ term(num ⦂ integer, num, emit_simple_term(num)),
          term(lit ⦂ boolean, [true]>>word(true), emit_simple_term(lit)),
          term(lit ⦂ boolean, [false]>>word(false), emit_simple_term(lit)),
          term(ident ⦂ a, word, emit_simple_term(ident)),
          expop(not ⦂ boolean → boolean, [[]>>lexeme(chrs('not')), subexpr],
                [_,[A],T]>>fmt_str(T, '(not ~w)', [A])),
          expop(and ⦂ boolean → boolean → boolean, infix(word(and)), emit_infix("and")),
          expop(or ⦂ boolean → boolean → boolean, infix(word(or)), emit_infix("or")),
          expop(xor ⦂ boolean → boolean → boolean, infix(tok(xor)), emit_infix("xor")),
          expop(ltlF ⦂ boolean → boolean,
                [[]>>lexeme(chrs('F(')), subexpr, lexeme(chrs(')'))],
                [_,[A],T]>>fmt_str(T, 'F(~w)', [A])),
          expop(ltlG ⦂ boolean → boolean,
                [[]>>lexeme(chrs('G(')), subexpr, lexeme(chrs(')'))],
                [_,[A],T]>>fmt_str(T, 'G(~w)', [A])),
          expop(ltlH ⦂ boolean → boolean,
                [[]>>lexeme(chrs('H(')), subexpr, lexeme(chrs(')'))],
                [_,[A],T]>>fmt_str(T, 'H(~w)', [A])),
          expop(ltlO ⦂ boolean → boolean,
                [[]>>lexeme(chrs('O(')), subexpr, lexeme(chrs(')'))],
                [_,[A],T]>>fmt_str(T, 'O(~w)', [A])),
          expop(ltlS ⦂ boolean → boolean → boolean,
                [[]>>lexme(chars('S(')), subexpr, []>>lexeme(chrs(',')),
                 subexpr, lexeme(chrs(')')),
                 % n.b. ltlS arguments in CoCoSpec helper format are reversed
                 % from LTL; the LTL ordering is canonical.
                 swapargs],
                [_,[A,B],T]>>fmt_str(T, 'S(~w, ~w)', [B,A])),
          expop(ltlSI ⦂ boolean → boolean → boolean,
                [[]>>lexme(chars('SI(')), subexpr, []>>lexeme(chrs(',')),
                 subexpr, lexeme(chrs(')')),
                 % n.b. ltlSI arguments in CoCoSpec helper format are reversed
                 % from LTL; the LTL ordering is canonical.
                 swapargs],
                [_,[A,B],T]>>fmt_str(T, 'SI(~w, ~w)', [B,A])),
          expop(ltlT ⦂ boolean → boolean → boolean,
                [[]>>lexme(chars('T(')), subexpr, []>>lexeme(chrs(',')), subexpr, lexeme(chrs(')'))],
                [_,[A,B],T]>>fmt_str(T, 'T(~w, ~w)', [A,B])),
          % TODO: ST (S Timed) and SIT (S Interval Timed)?
          expop(ltlU ⦂ boolean → boolean → boolean,
                [[]>>lexme(chars('U(')), subexpr, []>>lexeme(chrs(',')), subexpr, lexeme(chrs(')'))],
                [_,[A,B],T]>>fmt_str(T, 'U(~w, ~w)', [A,B])),
          expop(ltlUI ⦂ boolean → boolean → boolean,
                [[]>>lexme(chars('UI(')), subexpr, []>>lexeme(chrs(',')), subexpr, lexeme(chrs(')'))],
                [_,[A,B],T]>>fmt_str(T, 'UI(~w, ~w)', [A,B])),
          expop(ltlV ⦂ boolean → boolean → boolean,
                [[]>>lexme(chars('V(')), subexpr, []>>lexeme(chrs(',')), subexpr, lexeme(chrs(')'))],
                [_,[A,B],T]>>fmt_str(T, 'V(~w, ~w)', [A,B])),
          % no future-time specs (X) because, CoCoSpec is only past-time LTL.
          % KWQ: F, G?
          expop(ltlY ⦂ boolean → boolean,
                [[]>>lexeme(chrs('YtoPre(')), subexpr, lexeme(chrs(')'))],
                [_,[A],T]>>fmt_str(T, 'YtoPre(~w)', [A])),
          expop(ltlZ ⦂ boolean → boolean,
                [[]>>lexeme(chrs('ZtoPre(')), subexpr, lexeme(chrs(')'))],
                [_,[A],T]>>fmt_str(T, 'ZtoPre(~w)', [A])),
          expop(ltlF_bound ⦂ integer → boolean → boolean,
                [[]>>lexeme(chrs('FT(')), subexpr, lexeme(chrs(']')), subexpr],
                [_,[R,A],T]>>fmt_str(T, 'FT(~w, ~w)', [R, A])),
          expop(ltlG_bound ⦂ integer → boolean → boolean,
                [[]>>lexeme(chrs('GT(')), subexpr, lexeme(chrs(']')), subexpr],
                [_,[R,A],T]>>fmt_str(T, 'GT(~w, ~w)', [R, A])),
          expop(ltlH_bound ⦂ integer → boolean → boolean,
                [[]>>lexeme(chrs('HT(')), subexpr, lexeme(chrs(']')), subexpr],
                [_,[R,A],T]>>fmt_str(T, 'HT(~w, ~w)', [R, A])),
          expop(ltlO_bound ⦂ integer → boolean → boolean,
                [[]>>lexeme(chrs('OT(')), subexpr, lexeme(chrs(']')), subexpr],
                [_,[R,A],T]>>fmt_str(T, 'OT(~w, ~w)', [R, A])),
          expop(after ⦂ boolean → boolean,
                [[]>>lexeme(chrs('|>')), subexpr],
                [_,[A],T]>>fmt_str('(|> ~w)', [A])),
          expop(before ⦂ boolean → boolean,
                [[]>>lexeme(chrs('<|')), subexpr],
                [_,[A],T]>>fmt_str('(<| ~w)', [A])),
          expop(range_exact ⦂ integer → range,
                infix(chrs(',')),
                [_,[R],T]>>fmt_str(T, '~w, ~w', [R,R])),
          % n.b. range_exact parsing will supercede range_max_incl, range_max,
          % range_min_ncl, range_min, and range_min_max, but those constructors
          % are still replicated here for compatibility with the ltl expression
          % language parsing.
          expop(range_max_incl ⦂ integer → range,
                infix(chrs(',')),
                [_,[Max],T]>>fmt_str(T, '~w, 0', [Max])),
          expop(range_max ⦂ integer → range,
                infix(chrs(',')),
                [_,[Max],T]>>fmt_str(T, '(~w - 1), 0', [Max])),
          expop(range_min_incl ⦂ integer → range,
                infix(chrs(',')),
                [_,[Min],T]>>fmt_str(T, '0, ~w', [Max])),
          expop(range_min ⦂ integer → range,
                infix(chrs(',')),
                [_,[Min],T]>>fmt_str(T, '0, (~w - 1)', [Max])),
          expop(range_min_max ⦂ integer → integer → range,
                infix(chrs(',')),
                [_,[Min,Max],T]>>fmt_str(T, '~w, ~w', [Max, Min])),
          %% expop(exor ⦂ boolean → boolean → boolean, infix(word(xor)), emit_infix("xor")),
          expop(implies ⦂ boolean → boolean → boolean, infix(chrs('=>')), emit_infix("=>")),
          expop(eq ⦂ a → a → boolean, infix(chrs('=')), emit_infix("=")),
          expop(equiv ⦂ a → a → boolean, infix(chrs('=')), emit_infix("=")),
          expop(neq ⦂ a → a → boolean, infix(chrs('<>')), emit_infix("<>")),
          expop(gteq ⦂ integer → integer → boolean, infix(chrs('>=')), emit_infix(">=")),
          expop(lteq ⦂ integer → integer → boolean, infix(chrs('<=')), emit_infix("<=")),
          expop(gt ⦂ integer → integer → boolean, infix(chrs('>')), emit_infix(">")),
          expop(lt ⦂ integer → integer → boolean, infix(chrs('<')), emit_infix("<")),
          expop(neg ⦂ integer → integer, [[]>>lexeme(chrs('-')), subexpr],
                [_,[V],T]>>fmt_str(T, '(- ~w)', [V])),
          expop(add ⦂ integer → integer → integer, infix(chrs('+')), emit_infix("+")),
          expop(sub ⦂ integer → integer → integer, infix(chrs('-')), emit_infix("-")),
          expop(mul ⦂ integer → integer → integer, infix(chrs('*')), emit_infix("*")),
          expop(divd ⦂ integer → integer → integer, infix(chrs('/')), emit_infix("/")),
          expop(expo ⦂ integer → integer → integer, infix(chrs('^')), emit_infix("^"))
        ]}).
