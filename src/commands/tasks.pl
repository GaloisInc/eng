:- module(tasks, [ tasks_cmd/3, tasks_focus/1, tasks_help/1, tasks_help/2 ]).

:- use_module(library(ansi_term)).
:- use_module(library(strings)).
:- use_module('../englib').
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
|         taskID =
|           summary = ONE-LINE SUMMARY
|           description = FULL DESCRIPTION OF THE TASK
|           [priority = N]
|           [type = TASK TYPE]
|           [area = TASK AREA]
|           [severity = TASK SEVERITY]
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
|}.

%% ----------------------------------------

tasks_subcmds([list|todo]).

tasks_help(list, "List all tasks").
tasks_help(todo, "List tasks not yet completed").
% tasks_help(info, "Show task information").

:- dynamic exclude_task_status/1.

tasks_cmd(_, [list|_Args], 0) :- show_tasks(_) ; true.
tasks_cmd(_, [todo|_Args], 0) :- assertz(exclude_task_status(done)), show_tasks(_) ; true.
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
    format('  ~w~t~11|: [~w/~w]~`.t ~27|', [ TaskID, Pri, Sev ]),
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
