:- module(exprlang, [ define_language/2, show_language/1,
                      parse_expr/3, initial_gamma/1, expr/6,
                      op(750, xfy, →),
                      op(760, yfx, ⦂),
                      emit_expr/3, emit_simple_term/3, emit_infix/4,
                      % Helpers
                      num/3, word/3, lexeme/3, tok/3, chrs/3,
                      fmt_str/3
                    ]).

:- use_module(library(yall)).

:- dynamic language_name/2, type/2, atom/2, lang/2.

define_language(LangDef, Defs) :-
    get_dict(language, LangDef, LangName),
    get_dict(types, LangDef, LangTypes),
    get_dict(atoms, LangDef, LangAtoms),
    get_dict(phrases, LangDef, LangPhrases),
    retractall(language_name(LangName)),
    retractall(type(LangName, _)),
    retractall(atom(LangName, _)),
    retractall(lang(LangName, _)),
    asserta(language_name(LangName), LNDef),
    maplist([T,R]>>assertz(type(LangName, T), R), LangTypes, TypeDefs),
    maplist([A,R]>>assertz(atom(LangName, A), R), LangAtoms, AtomDefs),
    maplist([P,R]>>assertz(lang(LangName, P), R), LangPhrases, PhraseDefs),
    append([[LNDef], TypeDefs, AtomDefs, PhraseDefs], Defs).

show_language(Language) :-
    format('____ Language: ~w ____~n', [Language]),
    show_lang_types(Language),
    show_lang_atoms(Language),
    ( show_lang_expops(Language)
    ; format('____ <end of ~w definition> ____~n', [Language])
    ).
show_lang_types(Language) :-
    findall(T, type(Language, T), TS),
    format('◀ Types ▶ ~w~n', [TS]).
show_lang_atoms(Language) :-
    findall(T, atom(Language, T), TS),
    format('◀ Atoms ▶ ~w~n', [TS]).
show_lang_expops(Language) :-
    lang(Language, expop(Decl, infix(_Parser), _Emitter)),
    format('◀ infix Expr Op ▶ ~w~n', [ Decl ]),
    fail.
show_lang_expops(Language) :-
    lang(Language, expop(Decl, Parser, _Emitter)),
    is_list(Parser),
    format('◀ sqnce Expr Op ▶ ~w~n', [ Decl ]),
    fail.


%% ----------------------------------------------------------------------
%% Parsing the language


parse_expr(LangDef, Expr, ABT) :-
    string_chars(Expr, ECodes),
    enumerate(ECodes, Input),
    initial_gamma(Env),
    define_language(LangDef, _),
    get_dict(language, LangDef, Language),
    phrase(expr(Language, Env, ABT, _FinalEnv), Input).

expr(Language, Env, OutExpr, OutEnv) -->
    { lang(Language, expop(Op ⦂ OpType, OpParser, _)),
      is_list(OpParser)
    },
    exprParts(Language, Env, OpType, OpParser, [], Terms, Env1),
    { typecheck_expr(Language, Env1, OpType, Terms, [], Env2, OutTerms, OpOutType),
      Expr =.. [Op|OutTerms]
    },
    exprMore(Language, Env2, op(Expr, OpOutType), OutEnv, OutExpr).
expr(Language, Env, Expr, Env2) -->
    { lang(Language, term(TermID ⦂ TermType, TermParser, _)) },
    lexeme(call(TermParser, P)),
    typecheck(Language, Env, TermID, P, TermType, Env1, CheckedTermType),
    { Term =.. [TermID, P] },
    exprMore(Language, Env1, term(Term, CheckedTermType), Env2, Expr).
expr(Language, Env, Expr, OutEnv) -->
    lexeme(chrs('(')),
    expr(Language, Env, SubExpr, Env1),
    lexeme(chrs(')')),
    exprMore(Language, Env1, SubExpr, OutEnv, Expr).
expr(_, Env, end, Env) --> [].

exprMore(Language, Env, LeftTerm, OutEnv, op(Expr, OT)) -->
    { lang(Language, expop(Op ⦂ OpType, infix(OpParser), _)) },
    lexeme(call(OpParser)),
    lexeme(expr(Language, Env, RTP, Env1)),
    !,
    { typecheck_expr(Language, Env1, OpType, [LeftTerm, RTP], [],
                     OutEnv, [LT, RT], OT),
      Expr =.. [Op, LT, RT]
    }.
exprMore(_, Env, E, Env, E) --> [].
%% exprMore(_, Env, E, Env, _) -->
%%     any(20, V, P),
%%     { print_message(error, invalid_expr(E, V, P)), !, fail }.

exprParts(Language, Env, _ → TPS, [subexpr|Parsers], OpArgs,
          Terms, OutEnv) -->
    !,
    lexeme(expr(Language, Env, Arg, Env1)),
    typecheck(Language, Env1, Arg, Env2, TypedArg),
    % n.b. TParam will be checked against TypedArg after the entire expression is
    % parsed at the call site (above).
    exprParts(Language, Env2, TPS, Parsers, [TypedArg|OpArgs], Terms, OutEnv).
exprParts(Language, Env, TPS, [Parser|Parsers], OpArgs, Terms, OutEnv) -->
    call(Parser),
    exprParts(Language, Env, TPS, Parsers, OpArgs, Terms, OutEnv).
exprParts(_, Env, _OpType, [], OpArgs, Terms, Env) -->
    [], { reverse(OpArgs, Terms) }.

prolog:message(invalid_expr(E, V, P)) -->
    [ 'Expected more of expression "~w" @ offset ~w: ~w' - [E, P, V]].

%% ----------------------------------------

initial_gamma([]).  % list of (Name:str, type:atom)

typecheck(Language, Env1, term(Arg, ArgType), Env2, term(Arg, TermType)) -->
    { Arg =.. [TermID, Val] },
    typecheck(Language, Env1, TermID, Val, ArgType, Env2, TermType).

typecheck(_Language, Env, op(Op, OpType), Env, op(Op, OpType)) --> [].


typecheck(Language, Env, TermID, _Val, TermType, Env, TermType) -->
    { atom(Language, TermID),
      % Atom term, always valid (TermID and TermType came from Language)
      !
    }.
typecheck(Language, Env, _TermID, Val, TermType, OutEnv, TermType) -->
    { type(Language, TermType),
      % term has static type
      !,
      % ensure that if this static type either matches the var or assign this
      % static type to the vari if it has no assigned type.
      fresh_var(Env, Val, TermType, OutEnv)
    }.
typecheck(_Language, Env, _TermID, Val, _, OutEnv, ValType) -->
    % At this point missing the above indicates that TermType is a type variable.
    % The particular type variable can be discarded and instead this Val
    % (assumed--but not restricted--to be some sort of ident) should be
    % registered as fresh with an unsassigned type, so that future references to
    % Val have the opportunity to set the type.
    %% TODO: restrict this to 'ident' or equiv?.. could be a subexpr
    %% TODO: combine TermID and Val for more precise fresh_var matching?
    fresh_var(Env, Val, typevar, OutEnv, ValType).
%% typecheck(_Language, Env, TermID, Val, TermType, Env, TermType) -->
%%     { print_message(error, invalid_type(TermID, Val, TermType)), !, fail }.


typecheck_expr(Language, Env, Type → RType, [term(LT, Type)|Terms],
               TypeVars, OutEnv, [term(LT, Type)|OTerms], OType) :-
    type(Language, Type),
    % types match and this is a base type
    !,
    typecheck_expr(Language, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(Language, Env, Type → RType, [op(LT, Type)|Terms],
               TypeVars, OutEnv, [op(LT, Type)|OTerms], OType) :-
    type(Language, Type),
    % types match and this is a base type
    !,
    typecheck_expr(Language, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(Language, _, LType → _, [term(LT, LTType)|_], _, _, _, _) :-
    type(Language, LType),  % expr sig is fixed type
    type(Language, LTType), % term sig is also fixed type
    % LType and LTType are different fixed types (if they were the same type it
    % would have been matched above).
    !,
    print_message(error, invalid_term_type(LType, term(LT, LTType))),
    fail.
typecheck_expr(Language, _, LType → _, [op(LT, LTType)|_], _, _, _, _) :-
    type(Language, LType),  % expr sig is fixed type
    type(Language, LTType), % term sig is also fixed type
    % LType and LTType are different fixed types (if they were the same type it
    % would have been matched above).
    !,
    print_message(error, invalid_term_type(LType, term(LT, LTType))),
    fail.
%% typecheck_expr(Language, Env, LType → RType, [term(LT, LTType)|Terms],
%%                TypeVars, OutEnv, [term(LT, LType)|OTerms], OType) :-
%%     type(Language, LType),  % expr sig is fixed type
%%     % LType is fixed, but LTType is a variable (or it would have been matched
%%     % above)
%%     known_var(Env, LT, LType),
%%     % LTType variable is known to be the same as LType
%%     !,
%%     typecheck_expr(Language, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(Language, Env, LType → RType, [term(LT, LTType)|Terms],
               TypeVars, OutEnv, [term(LT, LTType)|OTerms], OType) :-
    type(Language, LTType),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above)
    member((LType, LTType), TypeVars), % expr type var has been assigned and matches
    !,
    typecheck_expr(Language, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(Language, Env, LType → RType, [op(LT, LTType)|Terms],
               TypeVars, OutEnv, [op(LT, LTType)|OTerms], OType) :-
    type(Language, LTType),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above)
    member((LType, LTType), TypeVars), % expr type var has been assigned and matches
    !,
    typecheck_expr(Language, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(Language, _, LType → _, [term(LT, LTType)|_], TypeVars, _, _, _) :-
    type(Language, LTType),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above)
    member((LType, OtherType), TypeVars),
    % expr type var has been assigned and does NOT match
    !,
    print_message(error, invalid_term_type(OtherType, term(LT, LTType))),
    fail.
typecheck_expr(Language, _, LType → _, [op(LT, LTType)|_], TypeVars, _, _, _) :-
    type(Language, LTType),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above)
    member((LType, OtherType), TypeVars),
    % expr type var has been assigned and does NOT match
    !,
    print_message(error, invalid_term_type(OtherType, term(LT, LTType))),
    fail.
typecheck_expr(Language, Env, LType → RType, [term(LT, LTType)|Terms],
               TypeVars, OutEnv, [term(LT, LTType)|OTerms], OType) :-
    type(Language, LTType),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above). Also know that LType has not been assigned otherwise one of the
    % previous two clauses would have matched.  Therefore, it's free and can be
    % assigned here.
    !,
    typecheck_expr(Language, Env, RType, Terms, [(LType, LTType)|TypeVars],
                   OutEnv, OTerms, OType).
typecheck_expr(Language, Env, LType → RType, [op(LT, LTType)|Terms],
               TypeVars, OutEnv, [op(LT, LTType)|OTerms], OType) :-
    type(Language, LTType),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above). Also know that LType has not been assigned otherwise one of the
    % previous two clauses would have matched.  Therefore, it's free and can be
    % assigned here.
    !,
    typecheck_expr(Language, Env, RType, Terms, [(LType, LTType)|TypeVars],
                   OutEnv, OTerms, OType).
typecheck_expr(Language, Env, LType → RType, [term(ident(LI), _LTType)|Terms],
               TypeVars, OutEnv, [term(ident(LI), AType)|OTerms], OType) :-
    % Neither LType nor LTType are fixed types, otherwise they would have been
    % matched above.
    member((LType, AType), TypeVars),
    !,
    % But LType has been assigned
    fresh_var(Env, LI, AType, Env2),
    % And LTType has already or just now been assigned to the same type
    !,
    typecheck_expr(Language, Env2, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(Language, Env, _LType → RType, [term(ident(LI), LTType)|Terms],
               TypeVars, OutEnv, [term(ident(LI), LTType)|OTerms], OType) :-
    % Neither LType nor LTType are fixed types, otherwise they would have been
    % matched above.  Also, LType has not been assigned a particular value.  In
    % this case, the type for the term is an existential; somewhat unusual, and
    % probably indicative that the term is unused, but it should be allowed
    % (e.g. const ⦂ a → b → a).
    typecheck_expr(Language, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(_, Env, OType, [], TypeVars, Env, [], RType) :-
    member((OType, RType), TypeVars),
    !.
typecheck_expr(_, Env, OType, [], _, Env, [], OType).
typecheck_expr(_, Env, OpType, Terms, _, Env, Terms, OpType) :-
    print_message(error, invalid_expr_types(OpType, Terms)),
    !,
    fail.

prolog:message(invalid_type(TermID, Val, TermType)) -->
    [ 'Invalid type for ~w of "~w"; expected type ~w' - [TermID, Val, TermType]].

prolog:message(invalid_term_type(WantedType, term(T, TermType))) -->
    [ 'Invalid term type: ~w ⦂ ~w but expected type ~w' - [T, TermType, WantedType]].
prolog:message(invalid_expr_types(TypeSpec, Terms)) -->
    { maplist([A,S]>>fmt_str(S,'    arg: ~w~n', [A]), Terms, ArgInfos),
      atomic_list_concat(ArgInfos, "", ArgInfo)
    },
    [ 'Invalid argument types for expression of type: ~w~n~w'
      - [ TypeSpec, ArgInfo ]].

% fresh_var/5 (DCG).  Called to check/save the new variable (with type determined
% by expression context) as a free variable, updating the Env and possibly
% returning the pre-determined type from the Env.
fresh_var(Env, VName, typevar, Env, Type) -->
    { known_var(Env, VName, Type), writeln(knownvar__same),
      % already saw this one, as a variable (up to alpha renaming)
      !
    }.
fresh_var(Env, VName, Type, Env, Type) -->
    { known_var(Env, VName, Type), writeln(knownvar__same),
      % already saw this one, same type
      ! }.
fresh_var(Env, VName, Type, Env, badtype) -->
    { known_var(Env, VName, VType), !,
      print_message(var_already_defined_with_other_type(VName, VType, Type)),
      fail
    }.
fresh_var(Env, VName, typevar, [var(VName, type_unassigned)|Env],
          type_unassigned) --> [].
fresh_var(Env, VName, VType, [var(VName, VType)|Env], VType) --> [].

% fresh_var/4 (non-DCG). Called when VName is seen as a term and the VType is
% known; this either verifies that the VName is already assigned this VType, or
% else it assigns the VType to the VName in an update environment.
fresh_var(Env, VName, VType, Env) :-
    member((VName, VType), Env),
    % VName is already assigned this VType
    !.
fresh_var(Env, VName, VType, [(VName, VType)|Env1]) :-
    selectchk(var(VName, type_unassigned), Env, Env1),
    % VName is known, now assigning the type.
    !.
fresh_var(Env, VName, NewType, Env) :-
    member((VName, VType), Env),
    !,
    print_message(error, cannot_reassign_type(VName, VType, NewType)),
    fail.

known_var(Env, VName, VType) :- member(var(VName, VType), Env).

prolog:message(var_already_defined_with_other_type(VName, VType, Type)) -->
    [ 'Variable "~w" already defined as ~w, cannot re-define as a ~w'
      - [ VName, VType, Type ]].


%% ----------------------------------------------------------------------

emit_expr(Language, term(P, type_unassigned), Expr) :-
    lang(Language, term(_Term ⦂ TermType, _, TermEmitter)),
    call(TermEmitter, term(P, TermType), Expr).
emit_expr(Language, term(P, TermType), Expr) :-
    lang(Language, term(_Term ⦂ _, _, TermEmitter)),
    call(TermEmitter, term(P, TermType), Expr).
emit_expr(Language, term(Op, _TermType), Expr) :-
    Op =.. [ BinOp, Arg1, Arg2 ],
    lang(Language, expop(BinOp ⦂ _, _, TermEmitter)),
    emit_expr(Language, Arg1, A),
    emit_expr(Language, Arg2, B),
    call(TermEmitter, BinOp, A, B, Expr).
emit_expr(Language, op(Op, _TermType), Expr) :-
    Op =.. [ Operator|Args ],
    lang(Language, expop(Operator ⦂ _Types, _, TermEmitter)),
    maplist(emit_expr(Language), Args, EArgs),
    call(TermEmitter, Operator, EArgs, Expr).
emit_expr(_, end, "") :- !.
emit_expr(_, ABT, Expr) :- fmt_str(Expr, '<<~w>>', [ABT]).

emit_simple_term(X, term(Y, _), T) :- Y =.. [X, A], fmt_str(T, '~w', A).
emit_infix(Repr, _, [LA, RA], T) :- fmt_str(T, '(~w ~w ~w)', [ LA, Repr, RA ]).

%% ----------------------------------------------------------------------
%% Parsing Helpers

enumerate(I, O) :- enum_(0, I, O).
enum_(_, [], []).
enum_(N, [I|IS], [(N,I)|OS]) :- succ(N, M), enum_(M, IS, OS).

fmt_str(V, Fmt, Args) :- format(atom(A), Fmt, Args), atom_string(A, V).

any(N, S, P) --> any_(N, L, P), {string_codes(S, L)}.
any_(N, [C|CS], P) --> [(P,C)], {succ(M, N)}, any_(M, CS, _).
any_(0, [], span(99999,99999)) --> [].
any_(_, [], span(99998,99998)) --> [].

word(W) --> [(_N,C)], { word_char(C), \+ char_type(C, digit) },
            wc(CS), { string_codes(W, [C|CS]) }.
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

lexeme(R) --> ws(_), !, lexeme(R).
lexeme(R) --> call(R).

tok(M) --> word(W), { any_case_match([A], W), atom_string(A,M) }.

any_case_match(Candidates, Word) :- to_lower(Word, LCWord),
                                    member(LCWord, Candidates).

to_lower(I, O) :- atom_string(IA, I), downcase_atom(IA, OA), atom_string(OA, O).

ws(span(N,N)) --> [(N,C)], { char_type(C, space) }.

chrs(Chars) --> { atom_chars(Chars, [C|CS]) }, [(_,C)], chrs_(CS).
chrs_([C|CS]) --> [(_,C)], chrs_(CS).
chrs_([]) --> [].
