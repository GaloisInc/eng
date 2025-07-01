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

:- dynamic language_name/2, type/2, atom/2, lang/2, variable_ref/2.

define_language(LangDef, Defs) :-
    get_dict(language, LangDef, LangName),
    get_dict(types, LangDef, LangTypes),
    get_dict(atoms, LangDef, LangAtoms),
    get_dict(phrases, LangDef, LangPhrases),
    get_dict(variable_ref, LangDef, LangVarRef),
    retractall(language_name(LangName)),
    retractall(type(LangName, _)),
    retractall(atom(LangName, _)),
    retractall(lang(LangName, _)),
    retractall(variable_ref(LangName, _)),
    asserta(language_name(LangName), LNDef),
    asserta(variable_ref(LangName, LangVarRef)),
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
    phrase(expr(Language, Env, ABT, _FinalEnv), Input, Rem),
    (Rem == [], !
    ; print_message(error, unparsed(Rem)), !, fail
    ).

prolog:message(unparsed(Remainder)) -->
    { enum_(CharNum, Codes, Remainder),
      string_chars(Text, Codes)
    },
    [ 'Parsing failed at position ~w: ~w~n' - [ CharNum, Text ] ].

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
exprMore(Language, Env, Expr, OutEnv, OutExpr) -->
    %% consume (trailing) whitespace
    ws(_),
    exprMore(Language, Env, Expr, OutEnv, OutExpr).
exprMore(_, Env, E, Env, E) --> [].
exprMore(_, Env, E, Env, _) -->
    any(20, V, P),
    { print_message(error, invalid_expr(E, V, P)), !, fail }.


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

initial_gamma(gamma{vartypes:[], bindnum:0}).  % list of (Name:str, type:atom)

pending_binding_type(Env, OutEnv, type_unassigned(T)) :-
    get_dict(bindnum, Env, N),
    succ(N, M),
    put_dict(bindnum, Env, M, OutEnv),
    format(atom(T), '⚲T~w', N).

% get the type of a term/op
type_of(term(_, T), T).
type_of(op(_, T), T).

% set the matched old type of a term/op (recursively) to the new type; no change
% if old type not matched.
set_type(M, T, term(A, M), term(A, T)) :- !.
set_type(_, _, term(A, O), term(A, O)).
set_type(M, T, op(A, M), op(ANew, T)) :-
    !,
    A =.. [Op|Args],
    maplist(set_type(M, T), Args, UpdArgs),
    ANew =.. [Op|UpdArgs].
set_type(M, T, op(A, O), op(ANew, O)) :-
    A =.. [Op|Args],
    maplist(set_type(M, T), Args, UpdArgs),
    ANew =.. [Op|UpdArgs].

% if this is a reference to a variable, return the name of the referenced variable
var_ref(Language, term(Term, _), V) :- variable_ref(Language, RS),
                                       writeln(var_ref__),
                                       member(R,RS),
                                       writeln(R),
                                       Term =.. [R, V].


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


%% typecheck_expr
%%
%% | exptype                | termtype | cmp | case | vars                  |
%% |------------------------+----------+-----+------+-----------------------|
%% | fixed → ...            | fixed    | ==  | tc1  |                       |
%% | fixed → ...            | fixed    | no  | tc2  |                       |
%% | typevar:assigned → ... | fixed    | ==  | tc3  |                       |
%% | typevar:assigned → ... | fixed    | no  | tc4  |                       |
%% | typevar → ...          | fixed    |     | tc5  | add typevar unassigne |
%% | typevar:assigned → ... | var      |     | tc6  | fresh var exptype     |
%% | typevar:assigned → ... | typevar  |     | tca  | use typevar:assigned  |
%% | typevar → ...          | var      |     | tc7  |                       |
%% | typevar:assigned [out] | --       |     | tc8  | return assigned       |
%% | fixed type [out]       | --       |     | tc9  | return fixed type     |
%% | other                  | other    |     |      | invalid_expr_types    |

typecheck_expr(Language, Env, Type → RType, [Term|Terms],
               TypeVars, OutEnv, [Term|OTerms], OType) :-
    % tc1
    type_of(Term, Type),
    type(Language, Type),
    % types match and this is a base type
    !,
    typecheck_expr(Language, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(Language, _, LType → _, [Term|_], _, _, _, _) :-
    % tc2
    type(Language, LType),  % expr sig is fixed type
    type_of(Term, LTType),
    type(Language, LTType), % term sig is also fixed type
    % LType and LTType are different fixed types (if they were the same type it
    % would have been matched above).
    !,
    print_message(error, invalid_term_type(LType, Term)),
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
typecheck_expr(Language, Env, LType → RType, [Term|Terms],
               TypeVars, OutEnv, [Term|OTerms], OType) :-
    % tc3
    type_of(Term, LTType),
    type(Language, LTType),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above)
    member((LType, LTType), TypeVars), % expr type var has been assigned and matches
    !,
    typecheck_expr(Language, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(Language, _, LType → _, [Term|_], TypeVars, _, _, _) :-
    % tc4
    type_of(Term, LTType),
    type(Language, LTType),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above)
    member((LType, OtherType), TypeVars),
    % expr type var has been assigned and does NOT match
    !,
    print_message(error, invalid_term_type(OtherType, Term)),
    fail.
typecheck_expr(Language, Env, LType → RType, [Term|Terms],
               TypeVars, OutEnv, [Term|OTerms], OType) :-
    % tc5
    type_of(Term, LTType),
    type(Language, LTType),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above). Also know that LType has not been assigned otherwise one of the
    % previous two clauses would have matched.  Therefore, it's free and can be
    % assigned here.
    !,
    typecheck_expr(Language, Env, RType, Terms, [(LType, LTType)|TypeVars],
                   OutEnv, OTerms, OType).
typecheck_expr(Language, Env, LType → RType, [Term|Terms],
               TypeVars, OutEnv, [OTerm|OTerms], OType) :-
    % tc6
    var_ref(Language, Term, LI),
    % Neither LType nor LTType are fixed types, otherwise they would have been
    % matched above, and Term is a variable.
    member((LType, AType), TypeVars),
    !,
    % But LType has been assigned
    fresh_var(Env, LI, AType, Env2),
    % And LTType has already or just now been assigned to the same type
    !,
    type_of(Term, LTType),
    set_type(LTType, AType, Term, OTerm),
    typecheck_expr(Language, Env2, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(Language, Env, LType → RType, [Term|Terms],
               TypeVars, OutEnv, [OTerm|OTerms], OType) :-
    % tca
    % Neither LType nor LTType are fixed types, otherwise they would have been
    % matched above.  Term is not a variable, but has a type driven by a variable.
    member((LType, AType), TypeVars),
    % LType has been assigned, and term has LType due to the op's type signature, so assign
    % Term the same AType.
    !,
    type_of(Term, LTType),
    set_type(LTType, AType, Term, OTerm),
    typecheck_expr(Language, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(Language, Env, LType → RType, [Term|Terms],
               TypeVars, OutEnv, [Term|OTerms], OType) :-
    % tc7
    % Neither LType nor LTType are fixed types, otherwise they would have been
    % matched above.  Also, LType has not been assigned a particular value.  In
    % this case, it may be that:
    %   * the type will be determined by subsequent terms,
    %   * that the overall type of the expression is indeterminate, or
    %   * that the type for the term is an existential; somewhat unusual, and
    %     probably indicative that the term is unused, but it should be allowed
    %     (e.g. const ⦂ a → b → a).
    type_of(Term, BType),
    typecheck_expr(Language, Env, RType, Terms, [(LType, BType)|TypeVars],
                   OutEnv, OTerms, OType).
typecheck_expr(_, Env, OType, [], TypeVars, Env, [], RType) :-
    % tc8
    member((OType, RType), TypeVars),
    !.
typecheck_expr(_, Env, OType, [], _, Env, [], OType).  % tc9: final known/fixed type
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
    { known_var(Env, VName, VType),
      !,
      print_message(var_already_defined_with_other_type(VName, VType, Type)),
      fail
    }.
fresh_var(Env, VName, typevar, OutEnv, BType) -->
    { get_dict(vartypes, Env, VT),
      pending_binding_type(Env, Env1, BType),
      put_dict(vartypes, Env1, [var(VName, BType)|VT], OutEnv)
    }.
fresh_var(Env, VName, VType, OutEnv, VType) -->
    { get_dict(vartypes, Env, VT),
      put_dict(vartypes, Env, [var(VName, VType)|VT], OutEnv)
    }.


% fresh_var/4 (non-DCG). Called when VName is seen as a term and the VType is
% known from previous information (although it could be a type_unassigned(X) with
% a the definitive type yet unknown); this either verifies that the VName is
% already assigned this VType, or else it assigns the VType to the VName in an
% update environment.
fresh_var(Env, VName, VType, Env) :-
    known_var(Env, VName, VType),
    % VName is already assigned this VType
    !.
fresh_var(Env, VName, type_unassigned(N), OutEnv) :-
    known_var(Env, VName, type_unassigned(T)),
    !,
    % Here, this variable is known but not its type, and now we have a new
    % unassigned type (i.e. the variable was used in a different part of the
    % expression where the type is not fixed but is driven from some other
    % portion of the expression).  The unassigned types must be unified; the
    % argument is an input only, so re-map all references to the previously-known
    % type to the new unassigned type.
    remap_fresh_var(Env, type_unassigned(T), type_unassigned(N), OutEnv).
fresh_var(Env, VName, VType, OutEnv) :-
    known_var(Env, VName, type_unassigned(T)),
    !,
    % This var was previously known but didn't have an actual type.  Now that we
    % have a type (which must be explicit since the previous match would have
    % taken care of an assignment to the same unassigned type), set that type for
    % *this* variable *and* all other variables with this unassigned type.
    remap_fresh_var(Env, type_unassigned(T), VType, OutEnv).
fresh_var(Env, VName, NewType, Env) :-
    known_var(Env, VName, VType),
    % previously checked for PrevType == NewType and for PrevType ==
    % type_unassigned(X), so here we know there is an invalid attempt to change
    % the variable's fixed type.
    !,
    print_message(error, cannot_reassign_type(VName, VType, NewType)),
    fail.
fresh_var(Env, VName, VType, OutEnv) :-
    % This var was previously unknown, so create an entry with a fresh unknown
    % type binding.
    get_dict(vartypes, Env, OldVars),
    put_dict(vartypes, Env, [var(VName, VType)|OldVars], OutEnv).


known_var(Env, VName, VType) :- get_dict(vartypes, Env, VarTypes),
                                member(var(VName, VType), VarTypes).


remap_fresh_var(Env, FromType, ToType, OutEnv) :-
    get_dict(vartypes, Env, OldVars),
    remap_fv_(OldVars, FromType, ToType, NewVars),
    put_dict(vartypes, Env, NewVars, OutEnv).
remap_fv_([], _, _, []).
remap_fv_([var(VName, FromType)|Vars], FromType, ToType, [var(VName, ToType)|OutVars]) :-
    !,
    remap_fv_(Vars, FromType, ToType, OutVars).
remap_fv_([V|Vars], FromType, ToType, [V|OVS]) :- remap_fv_(Vars, FromType, ToType, OVS).


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
