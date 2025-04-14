:- module(vctl, [ vctl_cmd/3, vctl_focus/1, vctl_help/1, vctl_help/2 ]).

:- use_module(library(strings)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(url)).
:- use_module(library(yall)).
:- use_module("../englib").
:- use_module('exec_subcmds').

vctl_focus("Version Control Engineering").

vctl_help(Info) :-
    engfile_dir(EngDirV),
    exec_subcmd_help("vctl", ExecHelpI),
    [EngDir, ExecHelp] = [EngDirV, ExecHelpI],
    Info = {|string(EngDir, ExecHelp)||
| Perform an VERSION CONTROL engineering task.
|
| Version control is usually handled by a tool such as git.  However, there
| may be pre- or post-checks or other operations that should be performed in
| conjunction with version control, there may be sub-projects that might or
| might not need to be locally present, and there may be configuration that
| needs to be performed based on the version control operations.
|
| There are typically a number of tools and capabilities built around the
| version control practices and these may vary by individual and may be
| quite complex.  The eng-based version control operations are designed
| to handle the simple, straightforward needs, and should be considered
| supplemental to using the version control tooling directly.
|
| Version control information is specified in one or more .eng files
| in the {EngDir} directory of the project.  These files are typically in
| EQIL format (see the EQIL design document), and have the following structure
| (uppercase is user-specified input):
|
|    vctl =
|      darcs =
|        complement = COMPLEMENT_REPO_PATHS
|      subproject =
|        NAME =
|          into = PATH
|          repo = REPO_URL
|          rev = BRANCH|TAG|REF
|
| In normal operations, the vctl command uses the repository information in
| the current working directory and needs no additional configuration.  At
| present, eng supports git and darcs.
|
| The darcs complement repo is optional. If the current project is using
| darcs for version control, the provided repository paths will automatically
| be supplied along with the --complement flag; this is useful for filtering
| out patches from the upstream repository that should not be considered for
| pulling into the local repository by moving them to the complement repository.
|
| The subproject is used to specify one or more dependencies for the current
| project.  This information is used in various ways, including: checking out
| the subproject locally for building, tracking dependent versions, etc.  The
| repo is required, into should be a relative directory (default: subproj/NAME)
| and rev is a specific commit reference in the target repo:
|    git: plain tag, branch, or patch
|    darcs: the match pattern or tag:X, hash:X, patch:X, or match:X
|
| The REPO_URL should be in a format recognized by a VCS tool.  The special
| prefix syntax "{SIBLING}/" followed by a repo name indicates that the name
| should be found at the same location (i.e. next to) the current repository.
|
| Frequently, a Personal Access Token (PAT) is needed to access private
| repositories or to avoid rate limiting.  A PAT can be set in an EQIL
| input file (usually in your $HOME/.config/eng directory) as:
|
|    vctl =
|      access token =
|        HOSTNAME = PAT
|        ...
|
|}.

vctl_help("status", "show status of local directory (workspace).").
vctl_help("push", "push local changes upstream.").
vctl_help("pull", "pull changes from upstream to local.").
vctl_help("subproj", "display sub-projects (dependencies).") :-
    eng:key(vctl, subproject).
vctl_help("subproj clone", "clone a dependency to a local sub-project.") :-
    eng:key(vctl, subproject).
vctl_help(SubCmd, Help) :- eng:key(vctl, subcmd, SubCmd, Help).

vctl_cmd(Context, [status|Args], Sts) :-
    vcs_tool(Context, VCTool), !,
    vctl_status(Context, VCTool, Args, Sts).
vctl_cmd(Context, [push|Args], Sts) :-
    vcs_tool(Context, VCTool), !,
    vctl_push(Context, VCTool, Args, Sts).
vctl_cmd(Context, [pull|Args], Sts) :-
    vcs_tool(Context, VCTool), !,
    vctl_pull(Context, VCTool, Args, Sts).

vctl_cmd(Context, [subproj], 0) :-
    vcs_tool(Context, VCTool), !,
    findall((S,L), eng:eng(vctl, subproject, S, into, L), SL),
    maplist(vctl_subproj_show(Context, VCTool), SL, SP),
    sum_list(SP, TSP),
    length(SL, NSL),
    format('Subprojects: ~w known, ~w present~n', [ NSL, TSP ]).

vctl_cmd(_, [subproj,clone], 1) :-
    findall(S, eng:key(vctl, subproject, S), []),
    print_message(error, no_subprojects()),
    !.
vctl_cmd(_, [subproj,clone], 1) :-
    findall(S, eng:key(vctl, subproject, S), SS),
    writeln('Please specify one or more subprojects to clone:'),
    maplist([S,O]>>format(atom(O), '  * ~w', [S]), SS, OS),
    intercalate(OS, '~n', OSS),
    writeln(OSS),
    writeln('  * ALL').
vctl_cmd(Context, [subproj,clone,'ALL'], Sts) :-
    vcs_tool(Context, VCTool), !,
    findall(E, (vctl_subproj_clone(Context, VCTool, _, E)), AllSts),
    sum_list(AllSts, Sts).
vctl_cmd(Context, [subproj,clone|Args], Sts) :-
    vcs_tool(Context, VCTool), !,
    findall(E, (member(N, Args), vctl_subproj_clone(Context, VCTool, N, E)), AllSts),
    sum_list(AllSts, Sts).

vctl_cmd(_, [Cmd|_], 1) :-
    member(Cmd, [ status, push ]), !,
    print_message(error, vcs_tool_undefined).
vctl_cmd(Context, [Cmd|Args], Sts) :-
    exec_subcmd_do(Context, vctl, Cmd, Args, Sts).
vctl_cmd(Context, [Cmd|_], invalid_subcmd(vctl, Context, Cmd)).

% ----------------------------------------------------------------------
%% Determine the VCS tool used for this project working directory.

% Find the VCS(s) applicable to the TopDir in the context.  Returns one of:
%
%   git(DIR)                    % DIR contains the .git directory
%   git(DIR, forge(URL, Auth))  % Like above, but with git remote origin info
%   darcs(DIR)                  % DIR contains the _darcs directory
%   darcs(DIR, git(..))         % Like above, but with the git backing dir (for dgsync)
%
vcs_tool(context(_, TopDir), GitTool) :-
    vcs_tool_git(context(_, TopDir), TopDir, GitTool).
vcs_tool(context(_, TopDir), DarcsTool) :-
    vcs_tool_darcs(context(_, TopDir), TopDir, DarcsTool).

vcs_tool_git(context(_, TopDir), InDir, git(InDir, forge(URL, Auth))) :-
    directory_file_path(InDir, ".git", VCSDir),
    exists_directory(VCSDir),
    do_exec(context(_, TopDir), 'vcs git remote origin', [ 'VCSDir' = InDir ],
            capture(["git", "-C", "{VCSDir}", "remote", "get-url", "origin"]),
            [], TopDir, [GitForgeURL|_]),
    atom_string(URLAtom, GitForgeURL),
    git_remote_url(URLAtom, URL),
    git_repo_PAT_hdr(URL, Auth).

vcs_tool_git(_Context, InDir, git(InDir)) :-
    directory_file_path(InDir, ".git", VCSDir),
    exists_directory(VCSDir).


vcs_tool_darcs(Context, InDir, darcs(InDir, GitTool)) :-
    directory_file_path(InDir, "_darcs", VCSDir),
    exists_directory(VCSDir),
    directory_file_path(VCSDir, "prefs", PrefsDir),
    directory_file_path(PrefsDir, "defaultrepo", ReposFile),
    exists_file(ReposFile),
    read_file_to_string(ReposFile, Contents, []),
    split_string(Contents, "\n", "", Repos),
    member(GitDir, Repos),
    vcs_tool_git(Context, GitDir, GitTool), !.

vcs_tool_darcs(_Context, InDir, darcs(InDir)) :-
    directory_file_path(InDir, "_darcs", VCSDir),
    exists_directory(VCSDir).

git_remote_url(Remote, URL) :- parse_url(Remote, URL).
git_remote_url(Remote, URL) :-
    string_concat("git@", Coord, Remote),
    split_string(Coord, ":", "", Parts),
    intercalate(Parts, "/", SlashRemote),
    string_concat("https://", SlashRemote, URLString),
    parse_url(URLString, URL).

%% git_repo_PAT_hdr(URL, [ 'PRIVATE-TOKEN'(PAT) ]) :-
git_repo_PAT_hdr(URL, [authorization(bearer(PAT))]) :-
    member(host(H), URL),
    atom_string(HA, H),
    eng:eng(vctl, 'access token', HA, PAT), !,
    print_message(information, using_pat(H)).
git_repo_PAT_hdr(_, []).

git_repo_path(URL, PathParts) :-
    member(path(P), URL),
    (string_concat(PS, ".git", P), !; PS = P),
    split_string(PS, "/", "", [_|PathParts]).

replace_url_path([], P, [path(P)]).
replace_url_path([path(_)|XS], P, URL) :- !, replace_url_path(XS, P, URL).
replace_url_path([X|XS], P, [X|URL]) :- replace_url_path(XS, P, URL).

replace_url_host([], H, [host(H)]).
replace_url_host([host(_)|XS], H, URL) :- !, replace_url_host(XS, H, URL).
replace_url_host([X|XS], H, [X|URL]) :- replace_url_host(XS, H, URL).

prolog:message(using_pat(H)) --> [ "Using PAT to access ~w~n" - [ H ] ].

% ----------------------------------------------------------------------
%% VCTL status command

vctl_status(context(EngDir, TopDir), git(VCSDir), _Args, Sts) :-
    do_exec(context(EngDir, TopDir), 'vcs git status', [ 'VCSDir' = VCSDir ],
            [ 'git -C {VCSDir} status -s',
              'git -C {VCSDir} fetch --dry-run origin -q'
            ],
            [], TopDir, Sts).
vctl_status(Context, git(VCSDir, forge(URL, Auth)), Args, Sts) :-
    vctl_status(Context, git(VCSDir), Args, Sts),
    directory_file_path(VCSDir, ".git", GitDir),
    directory_file_path(GitDir, "FETCH_HEAD", FHFile),
    read_file_to_string(FHFile, FHData, []),
    split_string(FHData, "\t ", "", [Fetch_SHA|_]),
    git_build_status_url(URL, Fetch_SHA, StatusURL),
    http_get(StatusURL, Data, [json_object(dict)|Auth]),
    (get_dict(status, Data, BldStatus)  % Gitlab
    ; (get_dict(workflow_runs, Data, []), BldStatus = "no CI"
      ; get_dict(workflow_runs, Data, WFRuns),
        reverse(WFRuns, [WFRun|_]),
        ( get_dict(status, WFRun, "completed"),
          get_dict(conclusion, WFRun, BldStatus)
        ; get_dict(status, WFRun,  BldStatus)
        )
      )
    ),
    format('build status = ~w~n', [ BldStatus ]).

vctl_status(context(EngDir, TopDir), darcs(VCSDir), _Args, Sts) :-
    darcs_pull_args(VCSDir, ExtraArgs),
    format(atom(PullCmd), 'darcs pull --repodir=~w -q --dry-run ~w',
           [VCSDir, ExtraArgs]),
    do_exec(context(EngDir, TopDir), 'vcs darcs status', [ 'VCSDir' = VCSDir ],
            [ PullCmd,
              'darcs push --repodir={VCSDir} -q --dry-run',
              'darcs w --repodir={VCSDir} -l'
            ],
            [], TopDir, Sts).

vctl_status(Context, darcs(DarcsDir, GitTool), Args, Sts) :-
    vctl_status(Context, GitTool, Args, GSts),
    vctl_status(Context, darcs(DarcsDir), Args, DSts),
    sum_list([DSts, GSts], Sts).

vctl_status(_Context, Tool, _Args, 1) :-
    print_message(error, unknown_vcs_tool(Tool)).

git_build_status_url(URL, _Fetch_SHA, StatusURL) :-
    member(host(H), URL),
    sub_string(H, _, _, _, "gitlab"),  %% <--- selector
    git_repo_path(URL, PathParts),
    intercalate(PathParts, "%2F", Project),
    intercalate(["api", "v4", "projects", Project,
                 "repository", "commits", "main" ], "/", SPath),
    string_concat("/", SPath, SPath2),
    string_concat(SPath2, "/", StatusPath),
    replace_url_path(URL, StatusPath, StatusURLP),
    parse_url(StatusURLS, StatusURLP),
    % The %2F is turned into %252F by parse_url... undo that
    sub_string(StatusURLS, SB, 3, SA, "%25"),
    sub_string(StatusURLS, 0, SB, _, SBS),
    string_concat(SBS, "%", SBM),
    sub_string(StatusURLS, _, SA, 0, SAS),
    string_concat(SBM, SAS, StatusURLR),
    atom_string(StatusURL, StatusURLR).

git_build_status_url(URL, Fetch_SHA, StatusURL) :-
    member(host(H), URL),
    sub_string(H, _, _, _, "github"),  %% <--- selector
    git_repo_path(URL, PathParts),
    intercalate(PathParts, "/", RepoPath),
    %% intercalate(["/repos", RepoPath, "commits", "HEAD", "status"], "/", SPath),
    intercalate(["/repos", RepoPath, "actions", "runs"], "/", SPath),
    replace_url_path(URL, SPath, StatusURLP),
    intercalate(["api", H], ".", APIHost),
    replace_url_host(StatusURLP, APIHost, StatusURLH),
    StatusURLQ = [search([exclude_pull_requests=true,
                          head_sha=Fetch_SHA
                         ])|StatusURLH],
    parse_url(StatusURLS, StatusURLQ),
    atom_string(StatusURL, StatusURLS).

darcs_remote_repo(VCSDir, RepoAddr) :-
    directory_file_path(VCSDir, "_darcs", DarcsDir),
    directory_file_path(DarcsDir, "prefs", PrefsDir),
    directory_file_path(PrefsDir, "defaultrepo", DefRepoFile),
    read_file_to_string(DefRepoFile, DefRepoStr, []),
    string_trim(DefRepoStr, RepoAddr).

darcs_pull_args(VCSDir, ExtraArgs) :-
    eng:key(vctl, darcs, complement),
    !,
    darcs_remote_repo(VCSDir, DefRepo),
    eng:eng(vctl, darcs, complement, ComplRepo),
    format(atom(ExtraArgs), '--complement ~w ~w', [DefRepo, ComplRepo]).
darcs_pull_args(_, "").


% ----------------------------------------------------------------------

vctl_push(context(EngDir, TopDir), git(VCSDir), _Args, Sts) :-
    !,
    do_exec(context(EngDir, TopDir), 'vcs git push', [ 'VCSDir' = VCSDir ],
            [ 'git -C {VCSDir} push origin'
            ],
            [], TopDir, Sts).
vctl_push(Context, git(VCSDir, forge(_,_)), Args, Sts) :-
    !,
    vctl_push(Context, git(VCSDir), Args, Sts).

vctl_push(context(EngDir, TopDir), darcs(VCSDir), _Args, Sts) :-
    !,
    do_exec(context(EngDir, TopDir), 'vcs darcs push', [ 'VCSDir' = VCSDir ],
            [ 'darcs push --repodir={VCSDir}'
            ],
            [], TopDir, Sts).

vctl_push(Context, darcs(DarcsDir, GitTool), Args, Sts) :- !,
    vctl_push(Context, darcs(DarcsDir), Args, DSts),
    format('Darcs pushes will need dgsync to add to git'),
    vctl_push(Context, GitTool, Args, GSts),
    sum_list([DSts, GSts], Sts).

vctl_push(_Context, Tool, _Args, 1) :-
    print_message(error, unknown_vcs_tool(Tool)).

% ----------------------------------------------------------------------

vctl_pull(context(EngDir, TopDir), git(VCSDir), _Args, Sts) :-
    !,
    do_exec(context(EngDir, TopDir), 'vcs git pull', [ 'VCSDir' = VCSDir ],
            [ 'git -C {VCSDir} pull origin'
            ],
            [], TopDir, Sts).
vctl_pull(Context, git(VCSDir, forge(_,_)), Args, Sts) :-
    !,
    vctl_pull(Context, git(VCSDir), Args, Sts).

vctl_pull(context(EngDir, TopDir), darcs(VCSDir), _Args, Sts) :-
    darcs_pull_args(VCSDir, ExtraArgs),
    format(atom(PullCmd), 'darcs pull --repodir=~w -q ~w',
           [VCSDir, ExtraArgs]),
    do_exec(context(EngDir, TopDir), 'vcs darcs pull', [ 'VCSDir' = VCSDir ],
            [ PullCmd ], [], TopDir, Sts).

vctl_pull(Context, darcs(DarcsDir, GitTool), Args, Sts) :-
    !,
    vctl_pull(Context, GitTool, Args, GSts),
    vctl_pull(Context, darcs(DarcsDir), Args, DSts),
    sum_list([DSts, GSts], Sts).

vctl_pull(_Context, Tool, _Args, 1) :-
    print_message(error, unknown_vcs_tool(Tool)).

% ----------------------------------------------------------------------
% vctl subproj helpers

% Get the preface to use for printing information about the named subproj
vctl_subproj_preface(Name, Preface) :-
    format(atom(Preface), '#__ ~w:: ', [Name]).

% Returns remote address: darcsremote(String), gitremote(parse_http URL, Auth),
% miscremote(String), or rmtNotSpecified.
vctl_subproj_remote_repo(VCTool, Name, Rmt) :-
    eng:eng(vctl, subproject, Name, repo, Repo),
    string_concat("{SIBLING}/", RepoName, Repo),
    !,
    vctl_repo_remote_addr(VCTool, RepoRemote),
    vctl_repo_addr_new_repo(RepoRemote, RepoName, Rmt).
vctl_subproj_remote_repo(_VCTool, Name, Rmt) :-
    eng:eng(vctl, subproject, Name, repo, miscremote(Rmt)), !.
vctl_subproj_remote_repo(_VCTool, _Name, rmtNotSpecified).

vctl_repo_remote_addr(git(_, forge(URL, Auth)), gitremote(URL, Auth)) :- !.
vctl_repo_remote_addr(darcs(VCSDir, _), darcsremote(Remote)) :-
    darcs_remote_repo(VCSDir, Remote), !.
vctl_repo_remote_addr(darcs(VCSDir), darcsremote(Remote)) :-
    darcs_remote_repo(VCSDir, Remote), !.
vctl_repo_remote_addr(VCTool, _) :-
    print_message(error, no_repo_remote(VCTool)),
    fail.

vctl_repo_addr_new_repo(gitremote(URL, Auth), RepoName, gitremote(NewURL, Auth)) :-
    member(path(Path), URL),
    split_string(Path, "/", "", [Project, _OldRepoName]),
    intercalate([Project, RepoName], "/", NewPath),
    replace_url_path(URL, NewPath, NewURL).
vctl_repo_addr_new_repo(darcsremote(URL), RepoName, darcsremote(NewURL)) :-
    split_string(URL, "/", "", Parts),
    reverse(Parts, [_|RBase]),
    reverse([RepoName|RBase], NewParts),
    intercalate(NewParts, "/", NewURL).

% Convert a vctl_subproj_remote_repo return into a printable string
vctl_subproj_remote_repo_str(rmtNotSpecified, "UNKNOWN") :- !.
vctl_subproj_remote_repo_str(darcsremote(S), R) :-
    string_concat("darcs ", S, R), !.
vctl_subproj_remote_repo_str(gitremote(URL, _), R) :-
    parse_http(URL, S),
    string_concat("git ", S, R), !.
vctl_subproj_remote_repo_str(miscremote(S), S) :- !.
vctl_subproj_remote_repo_str(Rmt, R) :- format(atom(R), '?? ~w', [Rmt]).

% Get the remote revision.  This may be specified via EQIL or may come from some
% other source (e.g. cabal.project, flake.lock, etc.).
vctl_subproj_remote_rev(Name, Rev) :-
    eng:eng(vctl, subproject, Name, rev, Rev).

% ----------------------------------------------------------------------

vctl_subproj_show(context(_, TopDir), VCTool, (Name, IntoDir), IsPresent) :-
    working_directory(_, TopDir),
    (exists_directory(IntoDir)
    -> IsPresent = 1, I=IntoDir
    ; IsPresent = 0,
      vctl_subproj_remote_repo(VCTool, Name, RmtAddr),
      vctl_subproj_remote_repo_str(RmtAddr, Rmt),
      (vctl_subproj_remote_rev(Name, Rev) ; Rev = ""),
      format(atom(I), "[currently @ ~w ~w]", [Rmt, Rev])
    ),
    vctl_subproj_preface(Name, Pfc),
    format('~w~w~n', [ Pfc, I ]).

% ----------------------------------------------------------------------

vctl_subproj_clone(Context, VCTool, DepName, CloneSts) :-
    eng:key(vctl, subproject, DepName),
    !,
    (eng:eng(vctl, subproject, DepName, into, TgtDir), ! ;
     format(atom(TgtDir), 'subproj/~w', [DepName])
    ),
    file_directory_name(TgtDir, TgtParentDir),
    ensure_dir_exists(Context, TgtParentDir),
    vctl_subproj_clone_into(Context, VCTool, DepName, TgtDir, CloneSts).
vctl_subproj_clone(context(EngDir, _TopDir), _, DepName, 1) :-
    print_message(error, unknown_subproject(EngDir, DepName)).

ensure_dir_exists(context(_, TopDir), TgtDir) :-
    format(atom(D), '~w/~w', [ TopDir, TgtDir ]),
    (exists_directory(D), ! ; make_directory(D)).

vctl_subproj_clone_into(_, _, DepName, TgtDir, 1) :-
    exists_directory(TgtDir),
    !,
    print_message(info, clone_target_exists(TgtDir, DepName)).
vctl_subproj_clone_into(Context, VCTool, DepName, TgtDir, CloneSts) :-
    vctl_subproj_remote_repo(VCTool, DepName, RmtRepo),
    (vctl_subproj_remote_rev(DepName, Rev), ! ; Rev = head),
    vctl_clone(Context, RmtRepo, Rev, TgtDir, CloneSts).

vctl_clone(context(EngDir, TopDir), darcsremote(Repo), head, TgtDir, 0) :-
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo ],
            [ 'darcs clone {RepoAddr} {TgtDir}' ],
            [], TopDir, 0), !.
vctl_clone(Context, darcsremote(Repo), Rev, TgtDir, Sts) :-
    split_string(Rev, ":", "", [T,V]),
    !,
    darcs_clone(Context, Repo, T, V, TgtDir, Sts).
vctl_clone(Context, darcsremote(Repo), Rev, TgtDir, Sts) :-
    !,
    darcs_clone(Context, Repo, "match", Rev, TgtDir, Sts).
vctl_clone(context(EngDir, TopDir), gitremote(URL, _), head, TgtDir, 0) :-
    parse_url(Repo, URL),
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo ],
            [ 'git clone {RepoAddr} {TgtDir}' ],
            [], TopDir, 0), !.
vctl_clone(context(EngDir, TopDir), gitremote(URL, _), Rev, TgtDir, 0) :-
    parse_url(Repo, URL),
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo, 'Ref' = Rev ],
            [ 'git clone {RepoAddr} {TgtDir}',
              'git checkout -C {TgtDir} {Ref}'
            ],
            [], TopDir, 0), !.
vctl_clone(_, Repo, _, TgtDir, 1) :-
    print_message(error, cannot_clone(Repo, TgtDir)).


darcs_clone(context(EngDir, TopDir), Repo, "tag", SelVal, TgtDir, Sts) :-
    !,
    format(atom(X), '-t ~w', [SelVal]),
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo, 'Ref' = X ],
            [ 'darcs clone {Ref} {RepoAddr} {TgtDir}' ],
            [], TopDir, Sts).
darcs_clone(context(EngDir, TopDir), Repo, "hash", SelVal, TgtDir, Sts) :-
    !,
    format(atom(X), '-to-hash=~w', [SelVal]),
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo, 'Ref' = X ],
            [ 'darcs clone {Ref} {RepoAddr} {TgtDir}' ],
            [], TopDir, Sts).
darcs_clone(context(EngDir, TopDir), Repo, "patch", SelVal, TgtDir, Sts) :-
    !,
    format(atom(X), '-to-patch=~w', [SelVal]),
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo, 'Ref' = X ],
            [ 'darcs clone {Ref} {RepoAddr} {TgtDir}' ],
            [], TopDir, Sts).
darcs_clone(context(EngDir, TopDir), Repo, "match", SelVal, TgtDir, Sts) :-
    !,
    format(atom(X), '-to-match=~w', [SelVal]),
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo, 'Ref' = X ],
            [ 'darcs clone {Ref} {RepoAddr} {TgtDir}' ],
            [], TopDir, Sts).
darcs_clone(_, _, Sel, SelVal, _, 1) :-
    print_message(error, unknown_darcs_revtype(Sel, SelVal)).

% ----------------------------------------------------------------------

prolog:message(unknown_vcs_tool(Tool)) -->
    [ 'Unknown VCS tool: ~w.  Unable to perform action' - [ Tool ] ].
prolog:message(vcs_tool_undefined) -->
    [ 'Unable to determine the VCS tool to use (e.g. git, darcs)' ].
prolog:message(unknown_subproject(EngDir, ProjName)) -->
    [ 'VCTL sub-project "~w" not defined in ~w files.' - [ ProjName, EngDir ] ].
prolog:message(clone_target_exists(TgtDir, FromLoc)) -->
    [ 'Cannot clone ~w into ~w: target directory already exists' - [ FromLoc, TgtDir ] ].
prolog:message(cannot_clone(Repo, TgtDir)) -->
    [ 'Failed to clone ~w into ~w' - [ Repo, TgtDir ] ].
prolog:message(no_repo_remote(VCTool)) --> ['No remote repo for ~w' - [VCTool]].
prolog:message(no_subprojects()) --> [ 'No subprojects are defined.' ].
prolog:message(unknown_darcs_revtype(Spec, Val)) -->
    [ 'Unknown revision specification type "~w" for: ~w' - [ Spec, Val ]].
