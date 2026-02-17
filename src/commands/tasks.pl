:- module(tasks, [ tasks_cmd/3, tasks_focus/1, tasks_help/1, tasks_help/2 ]).

:- use_module(library(ansi_term)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(strings)).
:- use_module('../englib').
:- use_module('../services/gitforge').
:- use_module(exec_subcmds).


tasks_focus("Tasking").

tasks_help(Info) :-
    engfile_dir(EngDirV),
    [EngDir] = [ EngDirV ],
    Info = {|string(EngDir)||
| Engineering TASK management.
|
| Tasks represent work that needs to be done.  These can include normal
| development of features/functionality, bug fixes, training, or anything
| else that has not yet been accomplished.  It is expected that Tasks are
| performed by human Engineers, although they may be accomplished via
| automation.
|
| Task information is specified in one or more .eng files in the {EngDir}
| directory of the project.  These files are typically in EQIL format
| (see the EQIL design document), and have the following structure:
|
|     tasks =
|       groupname
|         config =
|           [name = GroupName]
|           [priorities = 1 2 3 4 5 6 7 8 9 10]
|           [remote = SEE_BELOW]
|         taskID =
|           summary = ONE-LINE SUMMARY
|           description = FULL DESCRIPTION OF THE TASK
|           [priority = N]
|           [type = TASK TYPE]
|           [area = TASK AREA]
|           [severity = TASK SEVERITY]
|           [visibility = internal]
|           [effort = TASK EFFORT]
|           [status = TASK STATUS]
|
| Commonly, the taskID is left blank initially: ~eng~ will rewrite the
| task file and assign an unused task ID value.  The values of the status
| field should be on of: todo, in-progress, done (todo is assumed).
|
| The values of the other various
| optional fields are user-defined, although the following are recommended:
|
|   priority -- N is a value from 1 to 10, with 10 being "lower" priority
|   type -- one of: bug, feature enhancement, documentation, devops, design
|   area -- a functional area within the project that helps indicate where
|           the focus of the task work will be or what it will affect
|   severity -- low, medium, high, or a value 1-10 with 10 being highest
|   effort -- easy, moderate, difficult, or a numeric value with optional
|             units (recommended: h/hrs d/days, or w/wks).
|             If units are not supplied, it numeric values are assumed to
|             be a Kanban-style weight.
|
| Assigning a task an internal visibility will result in it not being
| suggested for inclusion in release notes.
|
| Each task group can have a remote specified; task information is synchronized
| between the local EQIL tasks and the remote task or issue information.
| The remote section has the following possible EQIL structure:
|
|     tasks =
|       groupname
|         config =
|           remote =
|             repo = PROJECT/REPO
|             [type = gitlab | github]
|             host = HOSTNAME
|             [label = COMMA-SEPARATED-LABELS]
|
| The label specifies any optional labels to use for issues synchronized
| here.  By default, the groupname is also used as a label.
|}.

%% ----------------------------------------

tasks_subcmds([list|todo]).

tasks_help(list, "List all tasks").
tasks_help(todo, "List tasks not yet completed").
tasks_help(sync, "Sync tasks with remote").
% tasks_help(info, "Show task information").

:- dynamic exclude_task_status/1.

tasks_cmd(_, [list|_Args], 0) :- show_tasks(_) ; true.
tasks_cmd(_, [todo|_Args], 0) :- assertz(exclude_task_status(done)), show_tasks(_) ; true.
tasks_cmd(_, [sync|_Args], Sts) :- sync_tasks(Sts); true.
tasks_cmd(_, [Cmd|_], invalid_subcmd(tasks, Cmd)) :- !.

%% ----------------------------------------

show_tasks(_Arg) :-
    eng:key(tasks, _),  % any known tasks?
    !,  % yes, there are some tasks
    findall(G, eng:key(tasks, G), Groups),
    member(Grp, Groups),
    task_group_name(Grp, Name),
    writeln(Name),
    task_group_priorities(Grp, Priorities),
    ( show_prioritized_tasks(Grp, Priorities)
    ; show_priority_unknown_tasks(Grp, Priorities)
    ).

show_prioritized_tasks(Grp, [Priority|_]) :-
    show_a_task(Grp, Priority).  % backtrace for each, then:
show_prioritized_tasks(Grp, [_|PS]) :-
    show_prioritized_tasks(Grp, PS). % move on to next priority

show_priority_unknown_tasks(Grp, Priorities) :- show_a_task_notpri(Grp, Priorities).

show_a_task(Grp, Pri) :-
    task_priority(Grp, TaskID, Pri),
    show_a_task(Grp, TaskID, Pri).

show_a_task(Grp, TaskID, Pri) :-
    eng:eng(tasks, Grp, TaskID, summary, Summary),
    task_severity(Grp, TaskID, Sev),
    task_status(Grp, TaskID, Sts, Colorized),
    enabled_task_status(Sts),
    format('  ~w~t~14|: [~w/~w]~`.t ~27|', [ TaskID, Pri, Sev ]),
    ansi_format(Colorized, Sts, []),
    write(' '), writeln(Summary),
    fail.  % next ...

show_a_task_notpri(Grp, Priorities) :-
    eng:key(tasks, Grp, TaskID),
    task_priority(Grp, TaskID, Pri),
    \+ member(Pri, Priorities),
    show_a_task(Grp, TaskID, Pri).


task_group_name(Grp, Name) :- eng:eng(tasks, Grp, config, name, Name), !.
task_group_name(Grp, Grp).

task_group_priorities(Grp, PS) :- eng:eng(tasks, Grp, config, priorities, P),
                                  !,
                                  split_string(P, " ", "", PS).
task_group_priorities(_, ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]).

task_severity(Grp, TaskID, Sev) :- eng:eng(tasks, Grp, TaskID, severity, Sev).
task_severity(Grp, TaskID, "medium") :- \+ eng:key(tasks, Grp, TaskID, severity).

task_priority(Grp, TaskID, Pri) :- eng:eng(tasks, Grp, TaskID, priority, Pri).
task_priority(Grp, TaskID, 5) :- \+ eng:key(tasks, Grp, TaskID, priority).

task_status(Grp, TaskID, Sts, Colorized) :-
    eng:eng(tasks, Grp, TaskID, status, S),
    !,
    task_colorize(S, Sts, Colorized).
task_status(_, _, Sts, Colorized) :- task_colorize("todo", Sts, Colorized).

task_colorize("todo", 'todo', [fg(yellow)]).
task_colorize("done", 'done', [bold, fg(black), bg(black)]).
task_colorize("in-progress", 'ACTV', [bold, fg(cyan)]).
task_colorize(O, S, [fg(red), bg(white)]) :-
    \+ member(O, ["todo", "done", "in-progress"]),
    format(atom(S), '~w?', [O]).

enabled_task_status(Sts) :- exclude_task_status(Sts), !, fail.
enabled_task_status(_).

% ----------------------------------------------------------------------

sync_tasks(Sts) :-
    eng:eng(tasks, Grp, config, remote, repo, RmtRepo),
    !,
    remote_updated(Grp, RmtRepo, Tasks),
    findall((T,S), sync_task(Grp, RmtRepo, T, S), TskSts),
    findall(T, new_remote_task(Grp, RmtRepo, Tasks, T), NewTasks),
    (NewTasks == 0, !;
     print_message(information, new_remote_tasks(NewTasks))
    ),
    !,
    (update_sync_timestamp(Grp), !;
     print_message(warning, unable_to_update_timestamp(Grp))
    ),
    NewTskSts = [],
    append(TskSts, NewTskSts, AllTskSts),
    maplist(sync_sts, AllTskSts, Sts).
sync_tasks(0).  % no remote configured for any tasks

prolog:message(new_remote_tasks(Tasks)) -->
    { length(Tasks, N) },
    [ 'There are ~w new remote tasks~n' - [N] ].
prolog:message(unable_to_update_timestamp(Grp)) -->
    [ 'Unable to update EQIL file timestamp for tasks ~w' - [Grp] ].

update_sync_timestamp(Grp) :-
    eng:eqil_file(tasks, Grp, config, File),
    !,
    get_time(TS),
    format_time(atom(Mark), '%FT%T%z', TS),
    update_eqilfile(File, [tasks, Grp, config, remote, '{updated at}'], Mark),
    !.
update_sync_timestamp(Grp) :-
    format('No eqil file for tasks ~w config~n', [Grp]).


sync_sts((_, S), S).

remote_id_key('{remote id}').

sync_task(Grp, RmtRepo, TaskID, Sts) :-
    eng:eng(tasks, Grp, TaskID, summary, _Summary), % set TaskID
    remote_id_key(RID_Key),
    (eng:eng(tasks, Grp, TaskID, RID_Key, RemoteID)
    -> sync_known_task(Grp, RmtRepo, TaskID, RemoteID, Sts)
    ; sync_new_task(Grp, RmtRepo, TaskID, Sts)).


% ----------------------------------------

sync_new_task(Grp, RmtRepo, TaskID, Sts) :-
    eng:eqil_file(tasks, Grp, TaskID, EQILFile),
    !,
    task_remote_summary(Grp, TaskID, Summary),
    task_remote_description(Grp, TaskID, Desc),
    create_remote_task(Grp, RmtRepo, Summary, Desc, RemoteID),
    (task_status(Grp, TaskID, done, _)
    -> format(atom(TIDStr), '~w', [RemoteID]),
       update_remote_if_changes(Grp, RmtRepo, TIDStr, [state_event=close])
    ; true
    ),
    print_message(info, new_task_remote_id(Grp, TaskID, RmtRepo, RemoteID)),
    remote_id_key(RID_Key),
    add_to_eqilfile(EQILFile, [tasks, Grp, TaskID], RID_Key, RemoteID),
    !,
    Sts = 0.
sync_new_task(Grp, _, TaskID, no_eqil_file_for_task(Grp, TaskID)).

prolog:message(no_eqil_file_for_task(Grp, TaskID)) -->
    [ 'No EQIL file found for task ~w ~w' - [Grp, TaskID] ].

prolog:message(new_task_remote_id(Grp, TaskID, RmtRepo, RmtID)) -->
    { eng:eng(tasks, Grp, config, remote, host, RmtHost) },
    [ 'Created new ~w ~w entry with remote ID ~w for task ~w ~w' -
      [ RmtHost, RmtRepo, RmtID, Grp, TaskID ] ].

create_remote_task(Grp, RmtRepo, Summary, Desc, RmtTaskID) :-
    build_mktask_url(Grp, RmtRepo, MakeTaskURL),
    git_repo_AUTH(MakeTaskURL, Auth),
    labels_string(Grp, Labels),
    append(MakeTaskURL,
           [ search([title=Summary,
                     description=Desc,
                     labels=Labels
                    ])
           ],
           PostURL),
    http_open(PostURL, STRM, [method(post)|Auth]),
    json_read_dict(STRM, RspDict),
    close(STRM),
    get_dict(iid, RspDict, RmtTaskID).

labels_string(Grp, Labels) :-
    eng:eng(tasks, Grp, config, remote, labels, GroupLabels),
    !,
    split_string(Grp, " ", " ", GrpWrds),
    intercalate(GrpWrds, "_", GrpLabel),
    intercalate([GrpLabel, GroupLabels], ", ", Labels).
labels_string(Grp, GrpLabel) :-
    split_string(Grp, " ", " ", GrpWrds),
    intercalate(GrpWrds, "_", GrpLabel).

build_mktask_url(Grp, RmtRepo, URL) :-
    build_task_url(Grp, RmtRepo, [], URL).


% ----------------------------------------

sync_known_task(Grp, RmtRepo, TaskID, RemoteID, 0) :-
    get_remote_task(Grp, RmtRepo, RemoteID, RmtInfo),
    !,
    sync_summary(Grp, TaskID, RmtInfo, [], UD0),
    sync_description(Grp, TaskID, RmtInfo, UD0, UD1),
    sync_status(Grp, TaskID, RmtInfo, UD1, UD2),
    % sync_severity(Grp, TaskID, RmtInfo, UD2, UD3), % not supported for gitlab
    update_remote_if_changes(Grp, RmtRepo, RemoteID, UD2),
    !.
sync_known_task(_Grp, RmtRepo, TaskID, RemoteID, cmd_not_impl(sync_known_task)) :-
    format('TBD: resync task ~w with remote ~w at ~w~n', [TaskID, RemoteID, RmtRepo]).


sync_summary(Grp, TaskID, RmtInfo, Pending, Pending) :-
    task_remote_summary(Grp, TaskID, Summary),
    get_dict(title, RmtInfo, Summary),
    !.
sync_summary(Grp, TaskID, RmtInfo, Pending, [title=Summary|Pending]) :-
    task_remote_summary(Grp, TaskID, Summary),
    print_message(informational, update_summary(Grp, TaskID, RmtInfo, Summary)).

prolog:message(update_summary(Grp, TaskID, RmtInfo, NewSumm)) -->
    { get_dict(title, RmtInfo, OldSumm),
      show_type(OldSumm, OT),
      show_type(NewSumm, NT),
      get_dict(iid, RmtInfo, RemoteID)
    },
    [ 'Updated ~w ~w summary (remote ID: ~w)~n  From(~w): ~w~n    To(~w): ~w' -
      [Grp, TaskID, RemoteID, OT, OldSumm, NT, NewSumm ] ].

sync_description(Grp, TaskID, RmtInfo, Pending, Pending) :-
    task_remote_description(Grp, TaskID, Description),
    get_dict(description, RmtInfo, Description),
    !.
sync_description(Grp, TaskID, RmtInfo, Pending, [description=D|Pending]) :-
    task_remote_description(Grp, TaskID, D),
    print_message(informational, update_description(Grp, TaskID, RmtInfo, D)).

task_remote_summary(Grp, TaskID, RmtSummary) :-
    eng:eng(tasks, Grp, TaskID, summary, Summary),
    format(string(RmtSummary), '[~w] ~w', [TaskID, Summary]).

task_remote_description(Grp, TaskID, RmtDesc) :-
    (eng:eng(tasks, Grp, TaskID, description, LocalDesc), !
    ; eng:eng(tasks, Grp, TaskID, summary, LocalDesc)),
    eng:eqil_file(tasks, Grp, TaskID, FileName),
    file_base_name(FileName, FName),
    format(string(RmtDesc), "~w~n~n[Task ~w in `~w`]",
           [ LocalDesc, TaskID, FName ]).

prolog:message(update_description(Grp, TaskID, RmtInfo, NewDesc)) -->
    { get_dict(description, RmtInfo, OldDesc),
      show_type(NewDesc, NT),
      show_type(OldDesc, OT),
      (NewDesc == OldDesc -> S="Same" ; S="Different"),
      get_dict(iid, RmtInfo, RemoteID)
    },
    [ 'Updated ~w ~w description [~w] (remote ID: ~w)~n  From(~w): ~w~n    To(~w): ~w' -
      [Grp, TaskID, S, RemoteID, OT, OldDesc, NT, NewDesc ] ].

sync_status(Grp, TaskID, RmtInfo, Pending, Pending) :-
    task_status(Grp, TaskID, Sts, _),
    get_dict(state, RmtInfo, RmtSts),
    same_status(Sts, RmtSts),
    !.
sync_status(Grp, TaskID, RmtInfo, Pending, [state_event=Sts|Pending]) :-
    task_status(Grp, TaskID, S, _),
    to_remote_status(Grp, RmtInfo, S, Sts),
    print_message(informational, update_status(Grp, TaskID, RmtInfo, Sts)).

same_status('done', "closed").
same_status('todo', "open").
same_status('todo', "opened").
same_status(A, S) :- atom_string(A, S).
same_status(_, "opened").  % Anything else we just treat as "open" on the gitlab side

to_remote_status(Grp, _, 'done', 'close') :-
    eng:eng(tasks, Grp, config, remote, type, "gitlab").
to_remote_status(Grp, _, 'todo', 'reopen') :-
    eng:eng(tasks, Grp, config, remote, type, "gitlab").

prolog:message(update_status(Grp, TaskID, RmtInfo, New)) -->
    { get_dict(state, RmtInfo, Old),
      get_dict(iid, RmtInfo, RemoteID)
    },
    [ 'Updated ~w ~w status (remote ID: ~w) from: ~w to ~w' -
      [Grp, TaskID, RemoteID, Old, New ] ].

update_remote_if_changes(_, _, _, []).
update_remote_if_changes(Grp, RmtRepo, RmtTaskID, Changes) :-
    build_task_url(Grp, RmtRepo, [RmtTaskID], URL),
    git_repo_AUTH(URL, Auth),
    append(URL, [ search(Changes) ], PostURL),
    http_open(PostURL, STRM, [method(put)|Auth]),
    json_read_dict(STRM, _Rspns),
    close(STRM).

get_remote_task(Grp, RmtRepo, RmtTaskID, Response) :-
    build_task_url(Grp, RmtRepo, [RmtTaskID], GetURL),
    git_repo_AUTH(GetURL, Auth),
    http_open(GetURL, STRM, [method(get)|Auth]),
    json_read_dict(STRM, Response),
    close(STRM).


% ----------------------------------------

build_task_url(Grp, RmtRepo, PathEnd, URL) :-
    eng:eng(tasks, Grp, config, remote, type, "gitlab"),
    !,
    eng:eng(tasks, Grp, config, remote, host, RmtHost),
    split_string(RmtRepo, "/", "", PathParts),
    intercalate(PathParts, "%2F", Project),
    append(["/api", "v4", "projects", Project, "issues"], PathEnd, PathElems),
    intercalate(PathElems, "/", Path),
    atom_string(PathA, Path),
    atom_string(RmtHostA, RmtHost),
    URL = [ protocol(https),
            host(RmtHostA),
            path(PathA)
          ].
build_task_url(Grp, _RmtRepo, _URL) :-
    eng:eng(tasks, Grp, config, remote, type, "gitlab"),
    !,
    fail. % TODO KWQ

% ----------------------------------------------------------------------

remote_updated(Grp, RmtRepo, Tasks) :-
    eng:eng(tasks, Grp, config, remote, type, "gitlab"),
    eng:eng(tasks, Grp, config, remote, '{updated at}', LastUpd),
    !,
    build_task_url(Grp, RmtRepo, [], URL),
    git_repo_AUTH(URL, Auth),
    append(URL, [ search = [ updated_after=LastUpd ] ], GetURL),
    http_open(GetURL, STRM, [method(get),
                             json_object(dict),
                             request_header('Accept'='application/json')
                             |Auth]),
    json_read_dict(STRM, Tasks),
    % TODO handle multiple pages of responses via pagination
    close(STRM).

remote_updated(_, _, []).

new_remote_task(Grp, RmtRepo, RmtTasks, TaskID) :-
    remote_id_key(RmtKey),
    member(RmtT, RmtTasks),
    get_dict(iid, RmtT, RmtTaskID),
    format(string(RTID), '~w', [RmtTaskID]),
    \+ eng:eng(tasks, Grp, TaskID, RmtKey, RTID),
    print_message(info, new_remote_task(RmtRepo, RmtTaskID)).

prolog:message(new_remote_task(RmtRepo, RmtTaskID)) -->
    [ 'New ~w task: ~w' - [RmtRepo, RmtTaskID] ].
