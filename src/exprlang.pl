:- module(exprlang, [ parse_expr/3,
                      op(750, xfy, →),
                      op(760, yfx, ⦂),
                      emit_expr/3, emit_simple_term/3, emit_infix/5,
                      % Helpers
                      num/3, word/3, lexeme/3, tok/3, chrs/3,
                      fmt_str/3
                    ]).

:- use_module(library(yall)).


parse_expr(LangDef, Expr, ABT) :-
    string_chars(Expr, ECodes),
    enumerate(ECodes, Input),
    initial_gamma(Env),
    phrase(expr(LangDef, Env, ABT, FinalEnv), Input).

expr(LangDef, Env, Expr, Env2) -->
    { get_dict(phrases, LangDef, LangPhrases),
      member(term(TermID ⦂ TermType, TermParser, _), LangPhrases)
    },
    lexeme(call(TermParser, P)),
    typecheck(LangDef, Env, TermID, P, TermType, Env1, CheckedTermType),
    { Term =.. [TermID, P] },
    exprMore(LangDef, Env1, term(Term, CheckedTermType), Env2, Expr).
expr(LangDef, Env, term(Expr, OutType), OutEnv) -->
    { get_dict(phrases, LangDef, LangPhrases),
      member(expop(Op ⦂ OpType, OpParser, _), LangPhrases),
      is_list(OpParser)
    },
    exprParts(LangDef, Env, OpType, OpParser, [], Terms, Env1),
    { typecheck_expr(LangDef, Env1, OpType, Terms, [], OutEnv, OutTerms, OutType),
      Expr =.. [Op|OutTerms]
    }.

expr(_, Env, end, Env) --> [].

exprMore(LangDef, Env, LeftTerm, OutEnv, term(Expr, OT)) -->
    { get_dict(phrases, LangDef, LangPhrases),
      member(expop(Op ⦂ OpType, infix(OpParser), _), LangPhrases)
    },
    lexeme(call(OpParser)),
    lexeme(expr(LangDef, Env, RTP, Env1)),
    !,
    typecheck_binexpr(LangDef, Env1, OpType, [LeftTerm, RTP], OutEnv, [LT, RT], OT),
    { Expr =.. [Op, LT, RT] }.
exprMore(_, Env, E, Env, E) --> [].
%% exprMore(_, Env, E, Env, _) -->
%%     any(20, V, P),
%%     { print_message(error, invalid_expr(E, V, P)), !, fail }.

exprParts(LangDef, Env, TParam → TPS, [subexpr|Parsers], OpArgs,
          Terms, OutEnv) -->
    !,
    lexeme(expr(LangDef, Env, term(Arg, ArgType), Env1)),
    { Arg =.. [TermID, P] },
    typecheck(LangDef, Env1, TermID, P, ArgType, Env2, CheckedArgType),
    % n.b. TParam will be checked against ArgType after the entire expression is
    % parsed at the call site (above).
    exprParts(LangDef, Env2, TPS, Parsers,
              [term(Arg, CheckedArgType)|OpArgs], Terms, OutEnv).
exprParts(LangDef, Env, TPS, [Parser|Parsers], OpArgs, Terms, OutEnv) -->
    call(Parser),
    exprParts(LangDef, Env, TPS, Parsers, OpArgs, Terms, OutEnv).
exprParts(_, Env, _OpType, [], OpArgs, Terms, Env) -->
    [], { reverse(OpArgs, Terms) }.

prolog:message(invalid_expr(E, V, P)) -->
    [ 'Expected more of expression "~w" @ offset ~w: ~w' - [E, P, V]].

%% ----------------------------------------

initial_gamma(env{ next_free:0,
                   known_vars:[],  % list of (Name:str, type:atom)
                   active_tvars:[]
                 }).

typecheck(LangDef, Env, TermID, _Val, TermType, Env, TermType) -->
    { get_dict(atoms, LangDef, LangAtoms),
      member(TermID, LangAtoms),
      % Atom term, always valid (TermID and TermType came from LangDef)
      !
    }.
typecheck(LangDef, Env, _TermID, Val, TermType, OutEnv, TermType) -->
    { get_dict(types, LangDef, LangTypes),
      member(TermType, LangTypes),
      % term has static type
      !,
      % ensure that if this static type either matches the var or assign this
      % static type to the vari if it has no assigned type.
      fresh_var(Env, Val, TermType, OutEnv)
    }.
typecheck(_LangDef, Env, _TermID, Val, TermType, OutEnv, ValType) -->
    % At this point missing the above indictes that TermType is a type variable
    %% TODO: combine TermID and Val for more precise fresh_var matching?
    fresh_var(Env, Val, typevar, OutEnv, ValType).
%% typecheck(_LangDef, Env, TermID, Val, TermType, Env, TermType) -->
%%     { print_message(error, invalid_type(TermID, Val, TermType)), !, fail }.


typecheck_binexpr(LangDef, Env, ExprType, Terms, OutEnv, OutTerms, OutType) -->
    { typecheck_expr(LangDef, Env, ExprType, Terms, [], OutEnv, OutTerms, OutType) }.

typecheck_expr(LangDef, Env, LType → RType, [term(LT, LType)|Terms],
               TypeVars, OutEnv, [term(LT, LType)|OTerms], OType) :-
    get_dict(types, LangDef, LangTypes),
    member(LType, LangTypes),
    !,
    typecheck_expr(LangDef, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(LangDef, _, LType → RType, [term(LT, LTType)|Terms],
               _, _, _, _) :-
    get_dict(types, LangDef, LangTypes),
    member(LType, LangTypes),  % expr sig is fixed type
    member(LTType, LangTypes), % term sig is also fixed type
    % LType and LTType are different fixed types (if they were the same type it
    % would have been matched above).
    !,
    print_message(error, invalid_term_type(LType, term(LT, LTType))),
    fail.
%% typecheck_expr(LangDef, Env, LType → RType, [term(LT, LTType)|Terms],
%%                TypeVars, OutEnv, [term(LT, LType)|OTerms], OType) :-
%%     get_dict(types, LangDef, LangTypes),
%%     member(LType, LangTypes),  % expr sig is fixed type
%%     % LType is fixed, but LTType is a variable (or it would have been matched
%%     % above)
%%     known_var(Env, LT, LType),
%%     % LTType variable is known to be the same as LType
%%     !,
%%     typecheck_expr(LangDef, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(LangDef, Env, LType → RType, [term(LT, LTType)|Terms],
               TypeVars, OutEnv, [term(LT, LTType)|OTerms], OType) :-
    get_dict(types, LangDef, LangTypes),
    member(LTType, LangTypes),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above)
    member((LType, LTType), TypeVars), % expr type var has been assigned and matches
    !,
    typecheck_expr(LangDef, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(LangDef, _, LType → RType, [term(LT, LTType)|Terms],
               TypeVars, _, _, _) :-
    get_dict(types, LangDef, LangTypes),
    member(LTType, LangTypes),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above)
    member((LType, OtherType), TypeVars),
    % expr type var has been assigned and does NOT match
    !,
    print_message(error, invalid_term_type(OtherType, term(LT, LTType))),
    fail.
typecheck_expr(LangDef, Env, LType → RType, [term(LT, LTType)|Terms],
               TypeVars, OutEnv, [term(LT, LTType)|OTerms], OType) :-
    get_dict(types, LangDef, LangTypes),
    member(LTType, LangTypes),  % term type is fixed
    % LTType is fixed, but LType is a variable (or it would have been matched
    % above). Also know that LType has not been assigned otherwise one of the
    % previous two clauses would have matched.  Therefore, it's free and can be
    % assigned here.
    !,
    typecheck_expr(LangDef, Env, RType, Terms, [(LType, LTType)|TypeVars],
                   OutEnv, OTerms, OType).
typecheck_expr(LangDef, Env, LType → RType, [term(ident(LI), LTType)|Terms],
               TypeVars, OutEnv, [term(ident(LI), AType)|OTerms], OType) :-
    % Neither LType nor LTType are fixed types, otherwise they would have been
    % matched above.
    member((LType, AType), TypeVars),
    !,
    % But LType has been assigned
    fresh_var(Env, LI, AType, Env2),
    % And LTType has already or just now been assigned to the same type
    !,
    typecheck_expr(LangDef, Env2, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(LangDef, Env, LType → RType, [term(ident(LI), LTType)|Terms],
               TypeVars, OutEnv, [term(ident(LI), LTType)|OTerms], OType) :-
    % Neither LType nor LTType are fixed types, otherwise they would have been
    % matched above.  Also, LType has not been assigned a particular value.  In
    % this case, the type for the term is an existential; somewhat unusual, and
    % probably indicative that the term is unused, but it should be allowed
    % (e.g. const ⦂ a → b → a).
    typecheck_expr(LangDef, Env, RType, Terms, TypeVars, OutEnv, OTerms, OType).
typecheck_expr(_, Env, OType, [], TypeVars, Env, [], RType) :-
    member((OType, RType), TypeVars), !.
typecheck_expr(_, Env, OType, [], TypeVars, Env, [], OType).
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
    [ 'Invalid argument types for binary expression of type: ~w~n~w'
      - [ TypeSpec, ArgInfo ]].

% fresh_var/5 (DCG).  Called to check/save the new variable (with type determined
% by expression context) as a free variable, updating the Env and possibly
% returning the pre-determined type from the Env.
fresh_var(Env, VName, typevar, Env, Type) -->
    { known_var(Env, VName, Type), ! }.   % already saw this one, same type
fresh_var(Env, VName, Type, Env, Type) -->
    { known_var(Env, VName, Type), ! }.   % already saw this one, same type
fresh_var(Env, VName, Type, Env, badtype) -->
    { known_var(Env, VName, VType), !,
      print_message(var_already_defined_with_other_type(VName, VType, Type)),
      fail
    }.
fresh_var(Env, VName, typevar, NewEnv, type_unassigned) -->
    { get_dict(known_vars, Env, KVs),
      put_dict(known_vars, Env, [var(VName, type_unassigned)|KVs], NewEnv)
    }.
fresh_var(Env, VName, VType, NewEnv, VType) -->
    { get_dict(known_vars, Env, KVs),
      put_dict(known_vars, Env, [var(VName, VType)|KVs], NewEnv)
    }.

% fresh_var/4 (non-DCG). Called when VName is seen as a term and the VType is
% known; this either verifies that the VName is already assigned this VType, or
% else it assigns the VType to the VName in an update environment.
fresh_var(Env, VName, VType, Env) :-
    get_dict(known_vars, Env, KVs),
    member((VName, VType), KVs),
    % VName is already assigned this VType
    !.
fresh_var(Env, VName, VType, NewEnv) :-
    get_dict(known_vars, Env, KVs),
    selectchk(var(VName, type_unassigned), KVs, NewKVs),
    % VName is known, now assigning the type.
    !,
    put_dict(known_vars, Env, [(VName, VType)|NewKVs], NewEnv).
fresh_var(Env, VName, NewType, Env) :-
    get_dict(known_vars, Env, KVs),
    member((VName, VType), KVs),
    !,
    print_message(error, cannot_reassign_type(VName, VType, NewType)),
    fail.

known_var(Env, VName, VType) :-
    get_dict(known_vars, Env, Known),
    member(var(VName, VType), Known).

prolog:message(var_already_defined_with_other_type(VName, VType, Type)) -->
    [ 'Variable "~w" already defined as ~w, cannot re-define as a ~w'
      - [ VName, VType, Type ]].


%% ----------------------------------------------------------------------

emit_expr(LangDef, term(P, type_unassigned), Expr) :-
    get_dict(phrases, LangDef, LangPhrases),
    member(term(_Term ⦂ TermType, _, TermEmitter), LangPhrases),
    call(TermEmitter, term(P, TermType), Expr).
emit_expr(LangDef, term(P, TermType), Expr) :-
    get_dict(phrases, LangDef, LangPhrases),
    member(term(_Term ⦂ _, _, TermEmitter), LangPhrases),
    call(TermEmitter, term(P, TermType), Expr).
emit_expr(LangDef, term(Op, _TermType), Expr) :-
    Op =.. [ BinOp, Arg1, Arg2 ],
    get_dict(phrases, LangDef, LangPhrases),
    member(expop(BinOp ⦂ _, _, TermEmitter), LangPhrases),
    emit_expr(LangDef, Arg1, A),
    emit_expr(LangDef, Arg2, B),
    call(TermEmitter, BinOp, A, B, Expr).
emit_expr(_, end, "") :- !.
emit_expr(_, ABT, Expr) :- fmt_str(Expr, '<<~w>>', [ABT]).

emit_simple_term(X, term(Y, _), T) :- Y =.. [X, A], fmt_str(T, '~w', A).
emit_infix(Repr, _, LA, RA, T) :- fmt_str(T, '~w ~w ~w', [ LA, Repr, RA ]).

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
