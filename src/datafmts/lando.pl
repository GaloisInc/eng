:- module(lando, [ process_lando_file/1,
                   parse_lando_file/2,
                   specElement_type/2,
                   specElement_ref/2
                 ]).

:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module('../englib').

process_lando_file(File) :-
    file_name_extension(_, ".lando", File),
    parse_lando_file(File, SSL),
    assertz(system_spec(SSL)),
    fail.  % to backtrack and find the next file.

parse_lando_file(File, SSL) :-
    access_file(File, read), !,
    print_message(informational, parsing_lando_file(File)),
    read_file_to_string(File, Contents, []),
    import_lando_from_string(File, Contents, SSL).
parse_lando_file(File, _{}) :-
    \+ access_file(File, read),
    print_message(warning, file_not_found(File)).

import_lando_from_string(Source, LandoText, Result) :-
    lando_ssl(Source, LandoText, Result, Leftover),
    import_warnings(Source, Leftover).

import_warnings(_, []) :- !.
import_warnings(Source, Leftover) :-
    print_message(warning, incomplete_lando_parse(Source, Leftover)).

lando_ssl(Source, InpString, Result, Leftover) :-
    string_chars(InpString, Chars),
    phrase(lexical_analysis(Source, Tokens), Chars, Leftover),
    parse_lando(Source, Tokens, Result).

parse_lando(Source, Tokens, Result) :-
    tmp_file_stream(FailFName, FailStrm, [encoding(utf8)]),
    ( phrase(ssl(FailStrm, Result), Tokens, Remainder)
    -> ((Remainder == []
        -> close(FailStrm)
        ; print_message(error, incomplete_lando_parse(Source, Remainder)),
          show_errs(FailStrm, FailFName),
          fail
        )
       )
    ; print_message(error, lando_parse_failure(Source)),
      show_errs(FailStrm, FailFName),
      Result = [],
      fail
    ).

show_errs(FailStrm, FailFName) :-
    close(FailStrm),
    read_file_to_string(FailFName, Fails, []),
    writeln(Fails).

% ----------------------------------------------------------------------
% Helpers for working with the resulting Lando

% Returns a capitalized string name for this element type.
specElement_type(D, "Imported Component") :- is_dict(D, componentImport), !.
specElement_type(D, T) :- is_dict(D, TA),
                          atom_chars(TA, [TAC0|TACR]),
                          upcase_atom(TAC0, TAC0U),
                          atom_string([TAC0U|TACR], T).

% Returns the abbrevName if set, otherwise the Name.
specElement_ref(D, R) :- get_dict(abbrevName, D, R), \+ R == null, !.
specElement_ref(D, R) :- get_dict(name, D, R).

% ----------------------------------------------------------------------

prolog:message(parsing_lando_file(FName)) -->
    [ 'Parsing Lando file ~w' - [FName] ].
prolog:message(file_not_found(FName)) -->
    [ 'Could not open Lando file ~w' - [FName] ].
prolog:message(incomplete_lando_parse(Source, SL)) -->
    { length(SL, SLLen) },
    [ 'No parse for trailing text "~w" in ~w  [~d following statements].'-[
          SL, Source, SLLen] ].
prolog:message(lando_tokenize_failure(Source)) -->
    [ 'Invalid characters in Lando specification in ~w failed~n' - [ Source ] ].
prolog:message(lando_parse_failure(Source)) -->
    [ 'Parsing of Lando specification in ~w failed~n' - [ Source ] ].
prolog:message(invalid_name(L, C)) -->
    [ 'Invalid name characters at line ~w, column ~w' - [ L, C ] ].

% --------------------------------------------------------------------------
% Tokenize a character stream read from a file.  These tokens are then fed to the
% parsing stage.  Tokenization largely removes whitespace and converts strings of
% characters into words.  It is also responsible for recording the source
% location of the tokens.

lexical_analysis(_, [T|Ts]) --> token(1, 0, T, NL, NC),
                                % n.b. lines start at 1, characters start at 0
                                lstream(NL, NC, Ts), !.
lexical_analysis(Source, _) -->
    { print_message(error, lando_tokenize_failure(Source)), fail }.

% The rest of the token stream, with whitespace separators when they are
% non-trivial.
lstream(L, C, [T|TS]) --> token(L, C, T, NL, NC), lstream(NL, NC, TS).
lstream(_, _, [eof]) --> [].

% Individual token recognition (make sure there's a token recognition for
% anything not accepted by word tokenizer w() below.
token(L, C, w(T, L, C), L, NC) --> w(W), !, { atom_chars(T, W),
                                              length(W, WC), plus(C, WC, NC) }.
token(L, C, comment(Cmnt, L, C), NL, NC) --> comment(L, C, Cmnt, NL, NC).
token(L, C, c('.',L,C), L, NC) --> ['.'], { succ(C, NC) }.
token(L, C, c(',',L,C), L, NC) --> [','], { succ(C, NC) }.
token(L, C, c(':',L,C), L, NC) --> [':'], { succ(C, NC) }.
token(L, C, c('!',L,C), L, NC) --> ['!'], { succ(C, NC) }.
token(L, C, c('?',L,C), L, NC) --> ['?'], { succ(C, NC) }.
token(L, C, c('(',L,C), L, NC) --> ['('], { succ(C, NC) }.
token(L, C, c(')',L,C), L, NC) --> [')'], { succ(C, NC) }.
token(L, C, c('/',L,C), L, NC) --> ['/'], { succ(C, NC) }. % after comment!
%% token(L, C, token_l, NL, NC) --> token(L, C, token_b, NL, NC), !.
token(L, C, token_l, NL, NC) --> token(L, C, token_n, NL, NC).
token(L, C, token_b, NL, NC) --> token(L, C, token_n, L1, C1),
                                 token(L1, C1, token_n, NL, NC).
token(L, _, token_n, NL, NC) --> [X], {char_type(X, end_of_line)}, !,
                                 { succ(L, L1) },
                                 skip_token_s(L1, 0, _, NL, NC).
token(L, C, token_s, L, NC) --> [X], { is_S(X), succ(C, NC) }.

skip_token_s(L, C, TSs, NL, NC) --> eat_token_s(L, C, TSs, NL, NC).
eat_token_s(L, C, [token_s|R], NL, NC) --> [X], { is_S(X) }, !,
                                           { succ(C, C1) },
                                           eat_token_s(L, C1, R, NL, NC).
eat_token_s(L, C, [], L, C) --> [].

is_S(' ').
is_S('\t').

% Create a word from a sequence of valid characters.  Note that a word can be
% implicitly preceeded by any number of token_s.
w([C|Cs]) --> [C], { word_char(C) }, wc(Cs).
wc([C|Cs]) --> [C], { word_char(C) }, !, wc(Cs).
wc([]) --> [].

word_char(C) :- \+ char_type(C, space),
                % Exclude things that might be individual tokens needing to be
                % recognized elsewhere in the grammar.
                \+ member(C, ['(', ')', '.', '!', '?', ':', ',',
                              %% '{', '}', '^', '[', ']', %% XXX?
                              '/']).

% Parse a comment (here to end-of-line)
comment(L, _, S, L, 0) --> ['/', '/'], white, whites, c2eol(CS), { atom_chars(S, CS) }.

c2eol([C|Cs]) --> [C], { \+ char_type(C, end_of_line) }, c2eol(Cs).
c2eol([]) --> [].

% --------------------------------------------------------------------------
% Parse tokenized Lando into the SSL IR. This closely follows the context-free
% grammar specified in grammar-v2.pdf in the develop docs of the Lando repo
% (https://gitlab-ext.galois.com/RDE/tools/lando).

ssl(FailStrm, Spec) -->
    lseq, body(FailStrm, 0, Bodies, _), lseq, clseq(C), lseq, [ eof ],
    { put_dict(_{comments:C}, _{body: Bodies}, Spec) }.

body(FailStrm, LastUID, [Body|Bodies], UID) -->
    { succ(LastUID, ThisUID) },
    specElement(FailStrm, ThisUID, Body, NextUID),
    body(FailStrm, NextUID, Bodies, UID).
body(_, LastUID, [], LastUID) --> [].


lseq --> sequence(lmatch, _).
lseqPlus --> lmatch(_), sequence(lmatch, _).
lmatch(token_l) --> sseq, [token_l], sseq.

clseq(CS) --> sequence(clmatch, CS).
clmatch(_{text:X, pos:_{line:L,col:C}}) --> sseq, [comment(X, L, C), token_l, token_l].
clmatch(_{text:X, pos:_{line:L,col:C}}) --> sseq, [comment(X, L, C), token_l].
clmatch_optional(CS) --> clmatch(C), {CS=[C]}; {CS=[]}.
cmnt_only(_{text:X, pos:_{line:L,col:C}}) --> sseq, [comment(X, L, C)].
cmnt_only_optional(CS) --> (sseq,
                            [comment(X, L, C)],
                            { CS = [ _{text:X, pos:_{line:L,col:C}} ] })
                           ; {CS = []}.

sseq --> sequence(smatch, _).
sseqPlus --> smatch(_), sequence(smatch, _).
smatch(token_s) --> [token_s].

specElement(FailStrm, TUID, S,  UID) --> system(FailStrm, TUID, S, UID).
specElement(FailStrm, TUID, S,  UID) --> subsystem(FailStrm, TUID, S, UID).
specElement(FailStrm, TUID, C,  UID) --> component(FailStrm, TUID, C, UID).
specElement(FailStrm, TUID, R,  UID) --> requirement(FailStrm, TUID, R, UID).
specElement(FailStrm, UID, ES, UID) --> events(FailStrm, UID, ES).
specElement(FailStrm, UID, SS, UID) --> scenarios(FailStrm, UID, SS).
specElement(FailStrm, UID, RS, UID) --> requirements(FailStrm, UID, RS).
specElement(FailStrm, UID, R,  UID) --> relation(FailStrm, UID, R).
specElement(FailStrm, UID, CI, UID) --> componentImport(FailStrm, UID, CI).
specElement(FailStrm, UID, _, UID) --> did_not_see(FailStrm, specElement).


system(FailStrm, UID, System, NextUID) -->
    lseq,
    clseq(ICS),
    [ w(system, SL, SC) ], !,
    saw(FailStrm, system, SL, system_(FailStrm, UID, ICS, SL, SC, System, NextUID)).
system_(FailStrm, UID, ICS, SL, SC, System, NextUID) -->
    sseq,
    name([], N, PMrk, _),
    optional(abbrev(A), {A=[_{abbrevName:null}]}), !,
    clmatch_optional(CLC),
    lseq,
    explanation(E),
    clseq(ELC),
    optional(indexing(FailStrm, I), {I=[_{indexing:[]}]}),
    optional(contains(FailStrm, UID, B, CCS, NextUID),
             { B=[], CCS=[], NextUID = UID }),
    { first_line_pos(SL, SC, ICS, P),
      mkElement(system, N, PMrk, P, UID, Elem0),
      addElemPart(_{explanation: E, body: B}, Elem0, Elem1),
      foldl(addElemPart, [A, I], Elem1, Elem2),
      foldl(extendElemPart(comments), [ICS, CLC, ELC, CCS], Elem2, System)
    }.

subsystem(FailStrm, UID, Subsys, NextUID) -->
    lseq,
    clseq(ICS),
    [ w(subsystem, SL, SC) ], !,
    saw(FailStrm, subsystem, SL, subsystem_(FailStrm, UID, ICS, SL, SC, Subsys, NextUID)).
subsystem_(FailStrm, UID, ICS, SL, SC, Subsys, NextUID) -->
    sseq,
    name([], N, PMrk, _),
    optional(abbrev(A), {A=[_{abbrevName:null}]}), !,
    % The following line is the only difference between system and subsystem
    optional(clientClause(C), {C=[_{clientOf:[]}]}),
    % clmatch_optional(CLC),
    cmnt_only_optional(CLC),
    lseq, !,
    explanation(E),
    clseq(ELC),
    optional(indexing(FailStrm, I), {I=[_{indexing:[]}]}),
    optional(contains(FailStrm, UID, B, CCS, NextUID),
             { B=[], CCS=[], NextUID = UID }),
    { first_line_pos(SL, SC, ICS, P),
      mkElement(subsystem, N, PMrk, P, UID, Elem0),
      addElemPart(_{explanation: E, body: B}, Elem0, Elem1),
      foldl(addElemPart, [A, C, I], Elem1, Elem2),
      foldl(extendElemPart(comments), [ICS, ELC, CLC, CCS], Elem2, Subsys)
    }.

component(FailStrm, UID, Comp, UID) -->
    lseq,
    clseq(ICS),
    [ w(component, SL, SC) ], !,
    saw(FailStrm, component, SL, component_(FailStrm, UID, ICS, SL, SC, Comp, UID)).
component_(FailStrm, UID, ICS, SL, SC, Comp, UID) -->
    sseq,
    name(["inherit", "client"], N, PMrk, _),
    optional(abbrev(A), {A=[_{abbrevName:null}]}), !,
    % TODO: support comments here?!
    relationClauses(RS),
    clmatch_optional(CLC),
    lseq,
    explanation(E),
    % clseq(ELC),
    componentFeatures(FailStrm, CF),
    optional(contains(FailStrm, UID, B, CCS, NextUID),
             { B=[], CCS=[], NextUID = UID }),
    { first_line_pos(SL, SC, ICS, P),
      mkElement(component, N, PMrk, P, UID, Elem0),
      addElemPart(_{explanation: E,
                    parts: B,
                    inherits: [],
                    clientOf: []}, Elem0, Elem1),
      foldl(addElemPart, [A, RS, CF], Elem1, Elem2),
      foldl(extendElemPart(comments), [ICS, CLC, CCS], Elem2, Comp)
    }.

componentImport(FailStrm, UID, CompImport) -->
    lseq,
    clseq(ICS),
    [ w(import, SL, SC) ], sseqPlus, [ w(component, _, _) ], !,
    saw(FailStrm, componentImport, SL, componentImport_(FailStrm, UID, ICS, SL, SC, CompImport)).
componentImport_(FailStrm, UID, ICS, SL, SC, CompImport) -->
    sseq,
    qname(["client"], N),
    optional(abbrev(A), {A=[_{abbrevName:null}]}), !,
    % TODO: support comments here?!
    sequence(clientClause, CLS),
    cmnt_only_optional(CLC),
    blockend(FailStrm),
    { first_line_pos(SL, SC, ICS, P),
      mkElement(componentImport, N, P, UID, Elem0),
      addElemPart(_{clientOf:[]}, Elem0, Elem1),
      foldl(addElemPart, [A, CLS], Elem1, Elem2),
      foldl(extendElemPart(comments), [ICS, CLC], Elem2, CompImport)
    }.

requirement(FailStrm, UID, Requirement, NextUID) -->
    lseq,
    clseq(ICS),
    %% Other than the name, this is the same as system
    [ w(requirement, SL, SC) ], !,
    saw(FailStrm, requirement, SL,
        requirement_(FailStrm, UID, ICS, SL, SC, Requirement, NextUID)).
requirement_(FailStrm, UID, ICS, SL, SC, Requirement, NextUID) -->
    sseq,
    name([], N, PMrk, _), !,
    optional(abbrev(A), {A=[_{abbrevName:null}]}), !,
    clmatch_optional(CLC),
    lseq,
    explanation(E),
    clseq(ELC),
    optional(indexing(FailStrm, I), {I=[_{indexing:[]}]}),
    optional(contains(FailStrm, UID, R, CCS, NextUID),
             { R=[], CCS=[], NextUID=UID }), !,
    { first_line_pos(SL, SC, ICS, P),
      mkElement(requirement, N, PMrk, P, UID, Elem0),
      addElemPart(_{explanation: E, requirements: R}, Elem0, Elem1),
      foldl(addElemPart, [A, I], Elem1, Elem2),
      foldl(extendElemPart(comments), [ICS , CLC, ELC, CCS], Elem2, Requirement)
    }.

events(FailStrm, UID, Events) -->
    lseq,
    clseq(ICS),
    [ w(events, SL, SC) ], !,
    saw(FailStrm, events, SL, events_(FailStrm, UID, ICS, SL, SC, Events)).
events_(FailStrm, UID, ICS, SL, SC, Events) -->
    commonEventReqScenario(FailStrm, events, UID, ICS, SL, SC, Events0, ES),
    { addElemPart(_{events: ES}, Events0, Events) }.

scenarios(FailStrm, UID, Scenarios) -->
    lseq,
    clseq(ICS),
    [ w(scenarios, SL, SC) ], !,
    saw(FailStrm, events, SL, scenarios_(FailStrm, UID, ICS, SL, SC, Scenarios)).
scenarios_(FailStrm, UID, ICS, SL, SC, Scenarios) -->
    commonEventReqScenario(FailStrm, scenarios, UID, ICS, SL, SC, Scenarios0, ES),
    { addElemPart(_{scenarios: ES}, Scenarios0, Scenarios) }.

requirements(FailStrm, UID, Requirements) -->
    lseq,
    clseq(ICS),
    [ w(requirements, SL, SC) ], !,
    saw(FailStrm, events, SL, requirements_(UID, ICS, SL, SC, Requirements)).
requirements_(FailStrm, UID, ICS, SL, SC, Requirements) -->
    commonEventReqScenario(FailStrm, requirements, UID, ICS, SL, SC, Requirements0, ES),
    { addElemPart(_{requirements: ES}, Requirements0, Requirements) }.

commonEventReqScenario(FailStrm, T, UID, ICS, SL, SC, Result, ES) -->
    sseq,
    name([], N, PMrk, _),
    clmatch_optional(CLC),
    lseq,
    sequence(commonEntry(FailStrm), ES),
    { first_line_pos(SL, SC, ICS, P),
      mkElement(T, N, PMrk, P, UID, Elem0),
      foldl(extendElemPart(comments), [ICS, CLC], Elem0, Result)
    }.

commonEntry(FailStrm, Entry) -->
    clseq(ICS),
    name([ "events",
           "scenarios",
           "requirements",
           "requirement",
           "component",
           "subsystem",
           "system",
           "relation",
           "import"
         ], N, P),
    {get_dict(line, P, Line)},
    saw(FailStrm, N, Line, commonEntry_(ICS, N, P, Entry)).
commonEntry_(ICS, N, P, Entry) -->
    cmnt_only_optional(CLC),
    lmatch(_),
    paragraph(D),
    { foldl(extendElemPart(comments), [ICS, CLC], _{id:N, text:D, pos:P}, Entry) }.

relation(FailStrm, UID, Relation) -->
    lseq,
    clseq(ICS),
    [ w(relation, SL, SC) ], !,
    saw(FailStrm, relation, SL, relation_(UID, ICS, SL, SC, Relation)).
relation_(UID, ICS, SL, SC, Relation) -->
    sseq,
    qname([], N),
    relationClause(R),
    relationClauses(RS),
    clmatch_optional(CLC),
    { first_line_pos(SL, SC, ICS, P),
      mkElement(relation, N, P, UID, Elem0),
      addElemPart(_{ inherits: [], clientOf: [] }, Elem0, Elem1),
      foldl(addElemPart, [R|RS], Elem1, Elem2),
      foldl(extendElemPart(comments), [ICS, CLC], Elem2, Relation)
    }.

relationClauses(RS) --> sequence(relationClause, RS).

relationClause(R) --> inheritClause(R).
relationClause(R) --> clientClause(R).

contains(FailStrm, UID, B, CS, NextUID) -->
    [ w(contains,_,_) ], !,
    lseq,
    body(FailStrm, UID, B, NextUID),
    [ w(end,_,_) ],
    %% clmatch_optional(_CLC), % TODO: comment discarded?
    optional((cmnt_only(C), {CS=[C]}), {CS=[]}),
    blockend(FailStrm).

componentFeatures(FailStrm, _{features:CF}) -->
    sequence(componentFeature(FailStrm), CF).

componentFeature(FailStrm, CF) --> constraint(CF), blockend(FailStrm).
componentFeature(FailStrm, CF) --> command(CF), blockend(FailStrm).
componentFeature(FailStrm, CF) --> query(CF), blockend(FailStrm).

constraint(CV) --> featureBody('.', CB), {addElemPart(CB, constraint{}, CV)}.
command(CV)    --> featureBody('!', CB), {addElemPart(CB, command{}, CV)}.
query(CV)      --> featureBody('?', CB), {addElemPart(CB, query{}, CV)}.

featureBody(E, CB) --> clseq(ICS),
                       sentBody(["system",
                                 "subsystem",
                                 "import", % Need 2nd word?
                                 "component",
                                 "events",
                                 "scenarios",
                                 "requirements",
                                 "requirement",
                                 "relation"
                                ], B, SP),
                       [ c(E,_,_) ],
                       featureEnd(CS),
                       { first_line_pos(SP, ICS, P),
                         string_concat(B, E, T),
                         CB = _{ pos: P, text: T, comments: CS }
                       }.

featureEnd(CS) --> sseq, optional((cmnt_only(C), {CS=[C]}), {CS=[]}).

name(Excluding, N, P) --> % no classification allowed (e.g. qname ref)
    sseq, nameWord(Excluding, X, P),
    sequence(moreName(Excluding), YS), sseq,
    { intercalate([X|YS], " ", N) }.
name(Excluding, N, PortionMark, P) -->
    sseq,
    [c('(',_,_)], fullWord("():", "):", PortionMark, _), [c(')',_,_)],
    sseq,
    nameWord(Excluding, X, P),
    sequence(moreName(Excluding), YS), sseq,
    { intercalate([X|YS], " ", N) }.
name(Excluding, N, no_portion_mark, P) --> % default portion mark
    sseq, nameWord(Excluding, X, P),
    sequence(moreName(Excluding), YS), sseq,
    { intercalate([X|YS], " ", N) }.
moreName(Excluding, Y) --> sseqPlus, nameWord(Excluding, Y, _).
abbrev(_{abbrevName:X}) --> [c('(',L,C)], !,
                            (nameWord([], X, _)
                            ; {print_message(warning, invalid_name(L, C))}
                            ), [c(')',_,_)].
qname(Excluding, [QNM|QNMS]) --> name(Excluding, QNM, _), [c(':',_,_)], !,
                                 qname(Excluding, QNMS).
qname(Excluding, [QNM]) --> name(Excluding, QNM, _).

nameWord(Excluding, X, P) --> fullWord("():", "():,.!?", X, P),
                              { \+ member(X, Excluding) }.

% Parse a word as limited by the Main Character Invalid (MCI) and Final Character
% Invalid (FCI) characters.  Nominally, this is just a w(W,_,_) lexical token,
% but some characters are tokenized separately, and those should be allowed as
% part of the parsed word as long as it's in the allowed portion, concatenating
% these together to create the full word to be parsed.
fullWord(MCI, FCI, W, _{line:L, col:C}) -->
    [w(A,L,C)],
    { main_does_not_contain(A, MCI) },
    remWord(MCI,FCI,WS), !,
    { string_concat(A, WS, W) }.
fullWord(MCI, FCI, W, _{line:L, col:C}) -->
    [c(A,L,C)],
    remWord(MCI,FCI,WS), !,
    { WS = "", ! ; does_not_contain(A, MCI, MCI) },
    { ok_ending(MCI, FCI, A, WS, W) }.

remWord(MCI, FCI, W) --> [ c(Char,_,_) ],
                         remWord(MCI, FCI, WS),
                         { ok_ending(MCI, FCI, Char, WS, W) }.
remWord(MCI, FCI, W) --> [ w(A,_,_) ],
                         { main_does_not_contain(A, MCI) },
                         remWord(MCI, FCI, WS),
                         { ok_ending(MCI, FCI, A, WS, W) }.
remWord(_, _, "") --> [].

ok_ending(MCI, FCI, This, "", This) :- does_not_contain(This, MCI, FCI).
ok_ending(MCI, _, This, Next, Out) :- \+ Next = "",
                                      string_concat(This, Next, Out),
                                      main_does_not_contain(Out, MCI).

clientClause(_{clientOf:[Q1|QS]}) --> sseq, [ w(client,_,_) ], sseq,
                                      qname(["inherits"], Q1), lseq,
                                      optional(moreQNames(["inherits"], QS), {QS=[]}).

inheritClause(_{inherits:[Q1|QS]}) --> sseq, [ w(inherit,_,_) ], sseq,
                                       qname(["client"], Q1), lseq,
                                       {QS = []}.
                                       optional(moreQNames(["client"], QS), {QS=[]}).

moreQNames(Excluding, QS) --> [c(',',_,_)], lseq,
                              sequence(qname(Excluding),
                                       ([lseq, c(',',_,_)], lseq), QS).

explanation(P) --> paragraph(P).

paragraph(P) --> sentence(S0), sequence(sentence, SS), parend(_),
                 { intercalate([S0|SS], " ", P) }.
sentence(S) --> sentBody(A), optional(sentTerm(B), {B=""}), wordSep(_),
                { string_concat(A,B,S) }.
sentBody(B) --> sentWord(W0, _), sequence(nextSentWord, WS),
                optional(wordSep(X), {X=[]}),
                { intercalate([W0|WS], " ", B) }.
sentBody(Excl, B, P) --> sentWord(W0, P),
                         { \+ member(W0, Excl) },
                         sequence(nextSentWord, WS),
                         optional(wordSep(X), {X=[]}),
                         { intercalate([W0|WS], " ", B) }.
sentWord(W, P) --> fullWord([], ['.', '!', '?'], W, P).
nextSentWord(W) --> wordSep(_), sentWord(W, _).
wordSep([]) --> sseqPlus.
wordSep([]) --> sseq, clmatch(_).
wordSep([]) --> sseq, lmatch(_).

sentTerm(".") --> [c('.',_,_)].
sentTerm("!") --> [c('!',_,_)].
sentTerm("?") --> [c('?',_,_)].
parend(C) --> clmatch_optional(C), [ token_l ], lseq.
parend(C) --> optional(cmnt_only(C), {C=[]}).

indexing(FailStrm, _{indexing:X}) -->
    [ w(indexing,_,_) ], !,
    sseq, sequence(indexEntry, X),
    blockend(FailStrm).

indexEntry(IndexEntry) -->
    [token_l], % lseq
    indexValue(K,P),
    [c(':',_,_)], !,
    indexValuePart(V,Cmnt),
    moreIndexValueParts(VS,CS),
    { foldl(extendElemPart(comments), [Cmnt, CS],
            _{key:K, values:[V|VS], pos:P}, IndexEntry) }.

indexValue(Val,P) --> sseq, indexValueWord(V,P), moreIndexValueWords(VS), sseq,
                      { intercalate([V|VS], " ", Val)}.

indexValueWord(W,P) --> fullWord([], [ ':' ], W, P).

moreIndexValueWords([V|VS]) --> sseqPlus, indexValueWord(V,_),
                                moreIndexValueWords(VS).
moreIndexValueWords([]) --> [].

%% indexValuePart(V,C) --> indexValue(V,_), lseq, clmatch_optional(C).

indexValuePart(V,C) --> clseq(_), indexValue(V,_),
                        cmnt_only_optional(C).

moreIndexValueParts([V|VS], CS) --> [ token_l ], % lseqPlus,
                                    indexValuePart(V, C),
                                    moreIndexValueParts(VS, MC),
                                    { append(C, MC, CS) }.
moreIndexValueParts([], []) --> [].

blockend(_) --> lseqPlus.
blockend(FailStrm) --> did_not_see(FailStrm, "blockend").

%% ----------------------------------------------------------------------

saw(_, _What, _Line, Body) --> call(Body), !.
saw(FailStrm, What, Line, _) -->
    [ At1, At2, At3 ],
    { format(FailStrm,
             '** Attempting "~w" [line ~w], failing at: ~w, ~w, ~w~n',
             [What, Line, At1, At2, At3]),
      fail
    }.
saw(FailStrm, What, Line, _) -->
    [ At1, At2 ],
    { format(FailStrm,
             '** Attempting "~w" [line ~w], failing at: ~w, ~w~n',
             [What, Line, At1, At2]),
      fail
    }.
saw(FailStrm, What, Line, _) -->
    [ At ],
    { format(FailStrm,
             '** Attempting "~w" [line ~w], failing at: ~w~n', [What, Line, At]),
      fail
    }.

did_not_see(FailStrm, What) -->
    [ At ],
    { format(FailStrm, '** Wanted "~w" but saw: ~w~n', [What, At]),
      fail
    }.
did_not_see(FailStrm, What) -->
    [ ],
    { format(FailStrm, '** Wanted "~w" but reached the end~n', [What]),
      fail
    }.

%% ----------------------------------------------------------------------

first_line_pos(SL, SC, [], _{line: SL, col: SC}).
first_line_pos(_, _, [C|_], P) :- get_dict(pos, C, P).
first_line_pos(SP, [], SP).
first_line_pos(_, [C|_], P) :- get_dict(pos, C, P).

mkElement(E, N, P, UID, E{ name: N, uid: UID, pos: P }).
mkElement(E, N, no_portion_mark, P, UID, E{ name: N, uid: UID, pos: P }).
mkElement(E, N, PM, P, UID, E{ name: N, portion_mark: PM, uid: UID, pos: P }).

% Adds P to dict E, where P is either a dict or a list of dicts
addElemPart(P, E, EOut) :-
    is_list(P), !, foldl(addElemPart, P, E, EOut)
    ; put_dict(P, E, EOut).

% Updates P in dict E at key N, where P is a dict or a list of dicts containing
% key N, and the at N is a list, where N in EOut is the concatenation of both
% existing E (if any) and P.
extendElemPart(N, P, E, EOut) :-
    get_dict(N, E, OldP), !, (append(OldP, P, NewP), put_dict(N, E, NewP, EOut))
    ; put_dict(N, E, P, EOut).

% Fails if the first string has overlaps with the second or third strings.
main_does_not_contain(S, Invalid) :-
    string_codes(S, SC),
    append(M, [_], SC),
    string_codes(Invalid, I),
    no_overlaps(M, I).

does_not_contain(S, InvalidMain, InvalidLast) :-
    string_codes(S, SC),
    append(M, [L], SC),
    string_codes(InvalidMain, IM),
    no_overlaps(M, IM),
    string_codes(InvalidLast, IL),
    no_overlaps([L], IL).

does_not_end_with(S, InvalidLast) :-
    string_codes(S, SC),
    append(_, [L], SC),
    string_codes(InvalidLast, I),
    no_overlaps([L], I).

% Takes two lists of string codes and is true if there are no common codes
no_overlaps(A, B) :- member(X, A), member(X, B), !, fail.
no_overlaps(_, _).
