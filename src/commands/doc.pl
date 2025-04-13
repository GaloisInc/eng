:- module(doc, [ doc_cmd/2, doc_cmd/3, doc_focus/1, doc_help/1, doc_help/2 ]).

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(strings)).
:- use_module('../englib').
:- use_module('../lando_tool').
:- use_module('exec_subcmds').


doc_focus("Documentation Engineering").

doc_help(Info) :-
    engfile_dir(EngDirV),
    [EngDir] = [ EngDirV ],
    Info = {|string(EngDir)||
| Perform a DOCUMENTATION engineering task.
|
| There are many different types of documentation that can be managed for
| a project, and some of the documentation operations overlap with other
| engineering aspects.  The doc subcommands provide the ability to find,
| view, and generate documentation.
|
| Documentation information is specified in one or more .eng files
| in the {EngDir} directory of the project.  These files are typically in
| EQIL format (see the EQIL design document), and have the following structure:
|
|     doc =
|       DOCNAME =
|         type = TYPE OF DOCUMENT (TYPICALLY 1 WORD, e.g. design, user-guide)
|         title = TITLE OF DOCUMENT
|         abstract = ABSTRACT (TYPICALLY 1 PARAGRAPH)
|         location = PATH_RELATIVE_TO_DIRECTORY_CONTAINING_{EngDir}
|}.

doc_help(list, "List the set of known documents").
doc_help(info, "Get summary information for the specified document").
doc_help(show, "Display the document via pager.").
doc_help(lando, "Run Lando requirements engineering operations").


%% ----------------------------------------

doc_cmd(_, [list|_Args], 0) :- writeln('Known documents:'), show_doc(_) ; true.

doc_cmd(_, [info], 1) :- print_message(error, specify_doc_id).
doc_cmd(_, [info,Doc|_Args], 0) :- ( doc_info(Doc), !
                                   ; print_message(error, doc_not_found(Doc))
                                   ).

doc_cmd(_, [show], 1) :- print_message(error, specify_doc_id).
doc_cmd(Context, [show,Doc|_Args], Sts) :-
    eng:eng(doc, Doc, location, Loc),
    !,
    directory_file_path(DocDir, DocFile, Loc),
    format(atom(InDir), "{TopDir}/~w", DocDir),
    format(atom(Cmd), 'less ~w~n', DocFile),
    do_exec(Context, "Document display", {}, [Cmd], [], InDir, Sts).
doc_cmd(_, [show,Doc|_Args], 1) :-
    eng:key(doc, Doc),
    !,
    print_message(error, no_doc_loc(Doc)).
doc_cmd(_, [show,Doc|_Args], 1) :-
    print_message(error, doc_not_found(Doc)).

doc_cmd(_, [Cmd|_], 1) :-
    print_message(error, invalid_doc_subcmd(Cmd)).

% Commands that do not use Context

doc_cmd([lando], 1) :- writeln({|string||
| Please specify the lando file and optionally the lando operation to perform.
|}
                       ).
doc_cmd([lando,SrcFile|LandoOp], Sts) :- !, lando(SrcFile, LandoOp, Sts).



%% ----------------------------------------

doc_subcmds([list, info, lando]).

show_doc(Doc) :-
    eng:key(doc, Doc),
    eng:eng(doc, Doc, title, Title),
    eng:eng(doc, Doc, type, Type),
    format('  ~w : ~w ~`-t ~w~76|~n', [ Doc, Title, Type ]),
    fail. % backtrack and try the next one

doc_info(Doc) :-
    eng:eng(doc, Doc, title, Title),
    eng:eng(doc, Doc, type, Type),
    (eng:eng(doc, Doc, abstract, Abstract); Abstract = ["None."]),
    (eng:eng(doc, Doc, location, Loc); Loc = "??"),
    format('~w ~w document:~n  Title: ~w~n  Location: ~w~n~n',
           [ Doc, Type, Title, Loc ]),
    %% Abstract
    format('  Abstract:~n  ---------~n', []),
    ( is_list(Abstract), !, write_strings("  ", Abstract)
    ; split_string(Abstract, "\n", "", Lines), !, write_strings("  ", Lines)
    ; write('  '), writeln(Abstract)
    ),
    %% Summary of other main-level elements in the doc
    findall(K, (eng:key(doc, Doc, K),
               \+ member(K, [ title, type, abstract, location ])
               ), Elems),
    (Elems = [], !
    ; format('~n  Other doc elements: ~w~n', [Elems])
    ).


summarize_all(Doc, Elems) :-
    member(Elem, Elems), summarize_each(Doc, Elem).

summarize_each(Doc, Elem) :-
    bagof(S, (eng:key(doc, Doc, Elem, S)), SS),
    length(SS, SL),
    format('  ~w~t~w~30|~n', [ Elem, SL ]),
    fail.  % next...


prolog:message(invalid_doc_subcmd(Cmd)) -->
    [ 'Invalid "doc" sub-command: ~w~n' - [ Cmd ] ],
    { known_subcommands(doc, CS) },
    [ 'Valid sub-commands: ~w~n' - [ CS ] ].
prolog:message(specify_doc_id) -->
    [ 'Please specify the document to show information for (see `eng doc list`)~n' - [] ].
prolog:message(no_doc_loc(Doc)) -->
    [ 'No location provided for document ~w~n' - [Doc] ].
prolog:message(doc_not_found(Doc)) -->
    [ 'Unknown document: ~w~n' - [Doc] ].
