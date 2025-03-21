:- module(lando_tool, [ lando/3,
                        write_lando_fret/2,
                        write_lando_fret_kind2/3,
                        write_lando_json/2,
                        write_lando_markdown/2
                      ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(http/json)).
:- use_module('src/datafmts/lando').
:- use_module('englib').
:- use_module(lando_fret).
:- use_module(fret_kind2).

lando(LandoSource, [], Sts) :-
    ( parse_lando_file(LandoSource, SSL), !,
      Sts = 0,
      get_dict(body, SSL, Body),
      get_dict_or(comments, SSL, [], Comments),
      length(Body, NumElements),
      length(Comments, NumComments),
      format('Parsed Lando file ~w: ~w elements & ~w comments.~n',
             [ LandoSource, NumElements, NumComments ])
    ; Sts = 1, !,
      print_message(error, parse_failure(LandoSource))
    ).

lando(LandoSource, ['to-json', OutFile], 0) :-
    parse_lando_file(LandoSource, SSL),
    open(OutFile, write, OutStrm),
    write_lando_json(OutStrm, SSL).

lando(LandoSource, ['to-markdown', OutFile], 0) :-
    parse_lando_file(LandoSource, SSL),
    open(OutFile, write, OutStrm),
    write_lando_markdown(OutStrm, SSL).

lando(LandoSource, ['to-fret', OutFile], 0) :-
    parse_lando_file(LandoSource, SSL),
    open(OutFile, write, OutStrm),
    write_lando_fret(OutStrm, SSL).

lando(LandoSource, ['to-fret-kind2', OutDir], 0) :-
    parse_lando_file(LandoSource, SSL),
    write_lando_fret_kind2(OutDir, SSL).

lando(LandoSource, _, 1) :-
    \+ parse_lando_file(LandoSource, _),
    print_message(error, parse_failure(LandoSource)).

lando(_, [], 1).  % failure if none of the above work

prolog:message(parse_failure(S)) --> [ 'Failed to parse ~w Lando file.' - [S] ].

%% ----------------------------------------------------------------------

write_lando_json(Strm, SSL) :- json_write_dict(Strm, SSL, [tag(type)]).

write_lando_fret(OutStrm, SSL) :-
    lando_to_fret(SSL, _, FretProject),
    json_write_dict(OutStrm, FretProject, []),
    format(OutStrm, '~n', []).

write_lando_fret_kind2(OutDir, SSL, OutFiles) :-
    lando_to_fret(SSL, Reqs, FretProject),
    fret_kind2(Reqs, FretProject, Kind2ConnComps),
    !,
    maplist(write_kind2(OutDir), Kind2ConnComps, OutFiles).

write_kind2(OutDir, Kind2Comp, Kind2FName) :-
    get_dict(compNum, Kind2Comp, CNum),
    get_dict(compName, Kind2Comp, PName),
    format(atom(FName), '~w_~w.lus', [ PName, CNum ]),
    directory_file_path(OutDir, FName, Kind2FName),
    open(Kind2FName, write, OutStrm),
    get_dict(kind2, Kind2Comp, Lustre),
    format(OutStrm, '~w~n', [Lustre]).

%% ----------------------------------------------------------------------

write_lando_markdown(Strm, SSL) :-
    get_dict(body, SSL, Body),
    phrase(lando_markdown(Body, [], BodyMD), Body, Remaining),
    ( Remaining = [], ! ;
      format('Unexpected extra not converted to Markdown: ~w~n', [Remaining])
    ),
    ( is_list(BodyMD), !, maplist(write_markdown(Strm), BodyMD)
    ; write_markdown(Strm, BodyMD)
    ).

write_markdown(Strm, MD) :- format(Strm, '~w~n', [MD]).

lando_markdown(AllElements, ParentNames, Msgs) -->
    [ SpecElement ],
    % Regular elements, defined in this file.  Notably, the Name is always
    % simple and never a QName, but another distinction would be the Type as
    % discriminated in the lando_markdown predicate below.
    { get_dict(name, SpecElement, Name),
      % relation allows a qname and is different, so don't process it here
      \+ is_list(Name), % !,
      %% get_dict(uid, SpecElement, UID),
      ( get_dict(portion_mark, SpecElement, PM)
      -> (PM == no_portion_mark
         -> PortionMark = "" % Allows defaulting/validation of parse
         ; format(atom(PortionMark), '(~w) ', [ PM ])
         )
      ; PortionMark = ""
      ),
      specElement_type(SpecElement, Type),
      specElement_ref(SpecElement, Ref),
      format(atom(StartMsg), "<!-- BEGIN ~w ~w -->", [ Type, Name ]),
      headerPrefix(ParentNames, Prefix),
      ( get_dict(abbrevName, SpecElement, AName), \+ AName == null, !,
        format(atom(Title), "~w <a id=\"~w\">~w~w: ~w (~w)</a>",
               [ Prefix, Ref, PortionMark, Type, Name, AName ])
      ; format(atom(Title), "~w <a id=\"~w\">~w~w: ~w</a>",
               [ Prefix, Ref, PortionMark, Type, Name ])
      ),
      ( ParentNames = [], !, ElemQName = []
      ; ( reverse([Name|ParentNames], QNameL),
          joinQName(AllElements, QNameL, QNameStr),
          format(atom(QNameMD), "- Qualified: *~w*~n", [QNameStr]),
          ElemQName = [QNameMD]
        )
      ),
      findall(P, specElement_markdown(AllElements, ParentNames, SpecElement, P),
              SEMsgsL),
      append(SEMsgsL, SEMsgs)
    },
    lando_markdown(AllElements, ParentNames, NextMsgs),
    { format(atom(EndMsg), "<!-- END ~w ~w -->", [ Type, Name ]),
      ( NextMsgs = [], !, SNextMsgs = NextMsgs ; SNextMsgs = [""|NextMsgs] ),
      append([[StartMsg,Title|ElemQName], SEMsgs,
              [EndMsg|SNextMsgs]], Msgs)
    }.

lando_markdown(AllElements, ParentNames, Msgs) -->
    [ SpecElement ],
    % Reference elements, not a local definition, but supplementary. These
    % typically have a QName rather than a Name, because they form a
    % reference and not a definition.
    { specElement_type(SpecElement, Type),
      member(Type, ["Relation", "Imported Component"]),
      get_dict(name, SpecElement, NameEntry),
      joinQName(AllElements, NameEntry, Name),
      format(atom(StartMsg), "<!-- BEGIN ~w ~w -->", [ Type, Name ]),
      headerPrefix(ParentNames, Prefix),
      ( get_dict(abbrevName, SpecElement, AName), \+ AName == null, !,
        format(atom(Title), "~w ~w: ~w (~w)", [ Prefix, Type, Name, AName ])
      ; format(atom(Title), "~w ~w: ~w", [ Prefix, Type, Name ])
      ),
      findall(P, specElement_markdown(AllElements, ParentNames, SpecElement, P),
              SEMsgsL),
      append(SEMsgsL, SEMsgs)
    },
    lando_markdown(AllElements, ParentNames, NextMsgs),
    { format(atom(EndMsg), "<!-- END ~w ~w -->", [ Type, Name ]),
      ( NextMsgs = [], !, SNextMsgs = NextMsgs ; SNextMsgs = [""|NextMsgs] ),
      append([[StartMsg,Title|SEMsgs], [EndMsg|SNextMsgs]], Msgs)
    }.

lando_markdown(_, _, ["<!-- Unknown entry -->"]) --> [ _ ].

lando_markdown(_, _, []) --> [].

specElement_markdown(_, _, SpecElement, [Expl, ""]) :-
    get_dict(explanation, SpecElement, Expl).

specElement_markdown(AllElements, _, SpecElement, Inherits) :-
    get_dict(inherits, SpecElement, IHL),
    specElement_inherits_markdown(AllElements, IHL, Inherits).

specElement_markdown(AllElements, _, SpecElement, ClientOf) :-
    get_dict(clientOf, SpecElement, CLL),
    specElement_clientOf_markdown(AllElements, CLL, ClientOf).

specElement_markdown(_, _, SpecElement, Indexes) :-
    get_dict(indexing, SpecElement, IDXS),
    maplist(specElement_index_markdown, IDXS, Indexes).

specElement_markdown(_, _, SpecElement, Features) :-
    get_dict(features, SpecElement, FL),
    maplist(specElement_feature_markdown, FL, Features).

specElement_markdown(AllElements, ParentNames, SpecElement, Requirements) :-
    get_dict(requirements, SpecElement, Reqs),
    get_dict(name, SpecElement, Name),
    phrase(lando_markdown(AllElements, [Name|ParentNames], Requirements), Reqs).

specElement_markdown(_, _, SpecElement, Events) :-
    get_dict(events, SpecElement, Evnts),
    maplist(specElement_evscen_markdown, Evnts, Events).

specElement_markdown(_, _, SpecElement, Scenarios) :-
    get_dict(scenarios, SpecElement, EL),
    maplist(specElement_evscen_markdown, EL, Scenarios).

% KWQ: dupe this for Component parts
specElement_markdown(AllElement, ParentNames, SpecElement, [""|Body]) :-
    get_dict(body, SpecElement, SubElems),
    get_dict(name, SpecElement, Name),
    \+ SubElems = [],
    phrase(lando_markdown(AllElement, [Name|ParentNames], Body), SubElems).

specElement_evscen_markdown(EventScenarioElement, MD) :-
    get_dict(id, EventScenarioElement, ID),
    get_dict(text, EventScenarioElement, Text),
    format(atom(MD),
           "<!-- BEGIN Item -->~n- **~w** ~w~n<!-- END Item -->",
          [ ID, Text ]).

specElement_index_markdown(IndexElement, IndexMD) :-
    get_dict(key, IndexElement, Key),
    get_dict(values, IndexElement, Values),
    ( Values = [ValueText], !, string_concat(" ", ValueText, Text)
    ; ( foldl(joinIndexValues, Values, [], ValueLines),
        foldl(string_concat, ValueLines, "", Text)
      )
    ),
    format(atom(IndexMD),
           "<!-- BEGIN Index Entry -->~n- Indexed **~w**:~w~n<!-- END Index Entry -->",
           [ Key, Text ]).

specElement_feature_markdown(FeatureElement, FeatureMD) :-
    specElement_type(FeatureElement, FType),
    get_dict(text, FeatureElement, Text),
    format(atom(FeatureMD),
           "<!-- BEGIN ~w -->~n- **~w**: ~w~n<!-- END ~w -->",
          [ FType, FType, Text, FType ]).


specElement_inherits_markdown(_, [], []).
specElement_inherits_markdown(AllElements, QNames, MDL) :-
    maplist(qNameEntry_markdown(AllElements, "Inherits"), QNames, MDL).

specElement_clientOf_markdown(_, [], []).
specElement_clientOf_markdown(AllElements, QNames, MDL) :-
    maplist(qNameEntry_markdown(AllElements, "Client of"), QNames, MDL).


%% ------------------------------

getRefToElement([E|AllElements], Name, Ref) :-
    get_dict(name, E, EName),
    (EName = Name, !, specElement_ref(E, Ref)
    ; (get_dict(abbrevName, E, AName), AName = Name, !, specElement_ref(E, Ref))
    ; (get_dict(body, E, Body), getRefToElement(Body, Name, Ref))
    ; (get_dict(parts, E, Parts), getRefToElement(Parts, Name, Ref))
    ; (get_dict(requirements, E, Reqs), getRefToElement(Reqs, Name, Ref))
    ; getRefToElement(AllElements, Name, Ref)
    ).

headerPrefix([], "##") :- !.
headerPrefix([_|R], Prefix) :- headerPrefix(R, HP),
                               string_concat("#", HP, Prefix).

joinIndexValues(Value, Lines, [VLine|Lines]) :-
    format(atom(VLine), "~n  - ~w", [Value]).

joinQName(AllElements, Names, Str) :-
    maplist(hrefName(AllElements), Names, Refs),
    intercalate(Refs, "**:**", Str), !.

hrefName(AllElements, Name, NameWithRef) :-
    getRefToElement(AllElements, Name, Ref),
    format(atom(NameWithRef), "[~w](#~w)", [Name, Ref]).
    % format(atom(NameWithRef), "<a href=\"#~w\">~w</a>", [Ref, Name]).

qNameEntry_markdown(AllElements, What, QName, MD) :-
    joinQName(AllElements, QName, QNameStr),
    format(atom(MD), "- ~w ~w", [ What, QNameStr ]).
