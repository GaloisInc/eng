:- module(vctl, [ vctl_cmd/3, vctl_focus/1, vctl_help/1, vctl_help/2,
                  vctl_help_internal/2,
                  % These are internal helpers for other modules to use:
                  vctl_subproj_local_dir/2,
                  vctl_subproj_remote_repo/3
                ]).

:- use_module(library(ansi_term)).
:- use_module(library(filesex)).
:- use_module(library(strings)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(ssl)).
:- use_module(library(url)).
:- use_module(library(yall)).
:- use_module('../englib').
:- use_module('../datafmts/cabal_project').
:- use_module(exec_subcmds).

vctl_focus("Version Control Engineering").

vctl_help(Info) :-
    engfile_dir(EngDirV),
    exec_subcmd_help("vctl", ExecHelpI),
    [EngDir, ExecHelp, SIBLING] = [EngDirV, ExecHelpI, 'SIBLING'],
    Info = {|string(EngDir, ExecHelp, SIBLING)||
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
|      git =
|        URL =
|          remote_CA = any_cert | file(FILENAME) | certificate(TEXT)
|      subproject =
|        NAME =
|          into = PATH
|          repo = REPO_URL
|          rev = BRANCH|TAG|REF
|          subdir = RELDIR
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
| The git URL setting is optional and can help specify certificate authorities
| if the target URL does not validate with a system-managed CA.  The "any_cert"
| setting bypasses verification of the SSL certificate; this is not generally
| adviseable but may be useful in situations where a private certificate is used.
|
| Subprojects:
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
| prefix syntax "{{SIBLING}}/" followed by a repo name indicates that the name
| should be found at the same location (i.e. next to) the current repository.
|
| The optional subdir specifies the subdirectory in the repo in which the
| NAME package exists.
|
| Repository Access:
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
vctl_help("subproj remove", "remove a local copy of a dependency.") :-
    eng:key(vctl, subproject).

vctl_help_internal("build_status", "show CI build status").

vctl_cmd(Context, [status|Args], [MainSts|SubSts]) :-
    vctl_subcmd(Context, vctl_status, Args, SubSts),
    vcs_tool(Context, VCTool), !,
    vctl_status(Context, VCTool, Args, MainSts), !.
vctl_cmd(Context, [push|Args], Sts) :-
    vcs_tool(Context, VCTool), !,
    vctl_push(Context, VCTool, Args, Sts), !.
vctl_cmd(Context, [pull|Args], Sts) :-
    vcs_tool(Context, VCTool), !,
    vctl_pull(Context, VCTool, Args, Sts), !.

vctl_cmd(Context, ['_',build_status|Args], [Sts|SubSts]) :-
    vctl_subcmd(Context, vctl_build_status, Args, SubSts),
    vcs_tool(Context, VCTool), !,
    vctl_build_status(Context, VCTool, Args, Sts).

vctl_cmd(Context, [subproj], sts(here, 0)) :-
    vcs_tool(Context, VCTool),
    setof((S,L), (eng:key(vctl, subproject, S),
                  vctl_subproj_local_dir(S, L)), SL),
    !,
    maplist(vctl_subproj_show(Context, VCTool), SL, SP),
    sum_list(SP, TSP),
    length(SL, NSL),
    format('Subprojects: ~w known, ~w present locally~n', [ NSL, TSP ]),
    !.
vctl_cmd(_, [subproj], unknown(here, no_subprojects)).

vctl_cmd(_, [subproj,clone], no_subprojects) :-
    findall(S, eng:key(vctl, subproject, S), []), !.
vctl_cmd(_, [subproj,clone], 1) :-
    setof(S, eng:key(vctl, subproject, S), SS),
    writeln('Please specify one or more subprojects to clone:'),
    maplist([S,O]>>format(atom(O), '  * ~w', [S]), SS, OS),
    intercalate(OS, '\n', OSS),
    writeln(OSS),
    writeln('  * ALL'), !.
vctl_cmd(Context, [subproj,clone,'ALL'], Sts) :-
    vcs_tool(Context, VCTool), !,
    findall(E, vctl_subproj_clone(Context, VCTool, _, E), Sts).
vctl_cmd(Context, [subproj,clone|Args], Sts) :-
    vcs_tool(Context, VCTool), !,
    findall(E, (member(N, Args), vctl_subproj_clone(Context, VCTool, N, E)), Sts).

vctl_cmd(_, [subproj,remove], no_subprojects) :-
    findall(S, eng:key(vctl, subproject, S), []), !.
vctl_cmd(_, [subproj,remove], 1) :-
    setof(S, D^(eng:key(vctl, subproject, S),
                vctl_subproj_local_dir(S, D),
                exists_directory(D)
               ), SS),
    (SS == []
    -> writeln('No subprojects locally cloned; nothing to remove.')
    ; writeln('Please specify one or more subprojects to remove:'),
      maplist([S,O]>>format(atom(O), '  * ~w', [S]), SS, OS),
      intercalate(OS, '\n', OSS),
      writeln(OSS),
      writeln('  * ALL')
    ), !.
vctl_cmd(Context, [subproj,remove,'ALL'], Sts) :-
    vcs_tool(Context, VCTool), !,
    findall(E, (vctl_subproj_remove(Context, VCTool, _, E)), Sts), !.
vctl_cmd(Context, [subproj,remove|Args], Sts) :-
    vcs_tool(Context, VCTool), !,
    findall(E, (member(N, Args), vctl_subproj_remove(Context, VCTool, N, E)), Sts).

vctl_cmd(context(_, TopDir), [Cmd|_], vcs_tool_undefined(TopDir)) :-
    member(Cmd, [ status, push ]), !.
vctl_cmd(Context, [Cmd|Args], Sts) :-
    exec_subcmd_do(Context, vctl, Cmd, Args, Sts).
vctl_cmd(Context, [Cmd|_], invalid_subcmd(vctl, Context, Cmd)).

% ----------------------------------------------------------------------
%% Determine the VCS tool used for this project working directory.

% Find the VCS(s) applicable to the TopDir in the context.  Returns one of:
%
%   git(DIR)                    % DIR contains the .git directory
%   git(DIR, forge(URL, Auth))  % Like above, but with git remote origin info
%                               % where Auth is http_get options
%   darcs(DIR)                  % DIR contains the _darcs directory
%   darcs(DIR, git(..))         % Like above, but with the git backing dir (for dgsync)
%
vcs_tool(context(C, TopDir), T) :- vcs_tool_in(context(C, TopDir), TopDir, T).

vcs_tool_in(Context, InDir, Tool) :- vcs_tool_git(Context, InDir, Tool).
vcs_tool_in(Context, InDir, Tool) :- vcs_tool_darcs(Context, InDir, Tool).

vcs_tool_git(context(_, TopDir), InDir, git(InDir, forge(URL, Auth))) :-
    directory_file_path(InDir, ".git", VCSDir),
    exists_directory(VCSDir),
    do_exec(context(_, TopDir), 'vcs git remote origin',
            [ 'VCSDir' = InDir ],
            capture(["git", "-C", "{VCSDir}", "remote", "get-url", "origin"]),
            [], TopDir, [GitForgeURL|_]),
    atom_string(URLAtom, GitForgeURL),
    git_remote_url(URLAtom, URL),
    git_repo_AUTH(URL, Auth).

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

git_repo_AUTH(URL, Auth) :-
    git_repo_PAT_hdr(URL, PAT),
    git_repo_cert(URL, Cert),
    append([PAT, Cert], Auth).

%% git_repo_PAT_hdr(URL, [ 'PRIVATE-TOKEN'(PAT) ]) :-
git_repo_PAT_hdr(URL, [authorization(bearer(PAT))]) :-
    member(host(H), URL),
    atom_string(HA, H),
    eng:eng(vctl, 'access token', HA, PAT), !,
    print_message(information, using_pat(H)).
git_repo_PAT_hdr(_, []).

git_repo_cert(URL, [cert_verify_hook(ssl:cert_accept_any)]) :-
    member(host(H), URL),
    atom_string(HA, H),
    eng:eng(vctl, git, HA, remote_CA, "any_cert"), !.
git_repo_cert(URL, [ca_certs([CA])]) :-
    member(host(H), URL),
    atom_string(HA, H),
    eng:eng(vctl, git, HA, remote_CA, CA), !.
git_repo_cert(_, []).

git_repo_path(URL, PathParts) :-
    member(path(P), URL),
    (string_concat(PS, ".git", P), !; PS = P),
    split_string(PS, "/", "", [_|PPS]),
    remove_blanks(PPS, PathParts).

remove_blanks([""|E], R) :- remove_blanks(E, R), !.
remove_blanks([], []).
remove_blanks([E|ES], [E|R]) :- remove_blanks(ES, R).

replace_url_path([], P, [path(P)]).
replace_url_path([path(_)|XS], P, URL) :- !, replace_url_path(XS, P, URL).
replace_url_path([X|XS], P, [X|URL]) :- replace_url_path(XS, P, URL).

replace_url_host([], H, [host(H)]).
replace_url_host([host(_)|XS], H, URL) :- !, replace_url_host(XS, H, URL).
replace_url_host([X|XS], H, [X|URL]) :- replace_url_host(XS, H, URL).

prolog:message(using_pat(H)) --> [ "Using PAT to access ~w" - [ H ] ].


% ----------------------------------------------------------------------
%% VCTL status command

vctl_status(context(EngDir, TopDir), git(VCSDir), _Args, Sts) :-
    vctl_subproj_preface(TopDir, Preface),
    writeln(Preface),
    do_exec(context(EngDir, TopDir), 'vcs git status', [ 'VCSDir' = VCSDir ],
            [ 'git -C {VCSDir} status -s',
              'git -C {VCSDir} fetch --dry-run origin -q'
            ],
            [], TopDir, Sts).
vctl_status(Context, git(VCSDir, forge(URL, Auth)), Args, Sts) :-
    vctl_status(Context, git(VCSDir), Args, Sts),
    vctl_build_status(Context, git(VCSDir, forge(URL, Auth)), Args, _).

vctl_status(context(EngDir, TopDir), darcs(VCSDir), _Args, Sts) :-
    darcs_pull_args(VCSDir, ExtraArgs),
    format(atom(PullCmd), 'darcs pull --repodir=~w -q --dry-run ~w',
           [VCSDir, ExtraArgs]),
    do_exec(context(EngDir, TopDir), 'vcs darcs status', [ 'VCSDir' = VCSDir ],
            [ PullCmd,
              'darcs push --repodir={VCSDir} -q --dry-run',
              'darcs whatsnew --repodir={VCSDir} -l || true'  % returns 1 if no unrecorded changes
            ],
            [], TopDir, Sts).

vctl_status(Context, darcs(DarcsDir, GitTool), Args, Sts) :-
    vctl_status(Context, GitTool, Args, GSts),
    vctl_status(Context, darcs(DarcsDir), Args, DSts),
    sum_list([DSts, GSts], Sts).

vctl_status(_Context, Tool, _Args, 1) :-
    print_message(error, unknown_vcs_tool(Tool)).

vctl_build_status(Context, git(VCSDir, forge(URL, Auth)), _Args, 0) :-
    !,
    git_remote_head(Context, VCSDir, Fetch_SHA), % TODO if local git repo, doesn't get *current* remote head, just remote head from this revision!
    git_build_status_url(URL, Fetch_SHA, StatusURL),
    http_get(StatusURL, RData, [json_object(dict)|Auth]),
    % gitlab returns a list; just use the first by default.
    (is_list(RData) -> RData = [Data|_] ; Data = RData),
    (get_dict(status, Data, BldStatus)  % Gitlab
    ; (get_dict(workflow_runs, Data, []), BldStatus = "no CI"
      ; get_dict(workflow_runs, Data, WFRuns),  % Github
        reverse(WFRuns, [WFRun|_]),  % use the latest github workflow run
        ( get_dict(status, WFRun, "completed"),
          get_dict(conclusion, WFRun, BldStatus)
        ; get_dict(status, WFRun,  BldStatus)
        )
      )
    ),
    member(host(RH), URL),
    member(path(RP), URL),
    show_bld_status(RH, RP, BldStatus).
vctl_build_status(Context, darcs(_, Parent), Args, Sts) :-
    !, vctl_build_status(Context, Parent, Args, Sts).
vctl_build_status(_Context, _VCSTool, _Args, 0) :-
    writeln('No build status available').

show_bld_status(RH, RP, "success") :- show_bld_status_(RH, RP, [bold], "success"), !.
show_bld_status(RH, RP, "running") :- show_bld_status_(RH, RP, [bold, fg('#fcec03')], "running"), !.
show_bld_status(RH, RP, "pending") :- show_bld_status_(RH, RP, [bold, fg('#fc8403')], "pending"), !.
show_bld_status(RH, RP, "no CI") :- show_bld_status_(RH, RP, [fg('#a0a0a0')], "no CI"), !.
show_bld_status(RH, RP, S) :- show_bld_status_(RH, RP, [bold, fg(red)], S).
show_bld_status_(RH, RP, F, S) :-
    ansi_format(F, '~w~t~10|', [S]),
    format('~w ~w build status', [RH, RP]),
    writeln('').

git_remote_head(_, VCSDir, RmtHeadSHA) :-
    directory_file_path(VCSDir, ".git", GitDir),
    directory_file_path(GitDir, "FETCH_HEAD", FHFile),
    exists_file(FHFile),
    !,
    read_file_to_string(FHFile, FHData, []),
    split_string(FHData, "\t ", "", [RmtHeadSHA|_]).
git_remote_head(Context, VCSDir, RmtHead) :-
    % n.b. this is a fallback from the previous matching clause that is not
    % always reliable (the refs/remotes below does not always exist, and it's not
    % clear what a consistent ref would be); warn when using this method.
    print_message(warning, unreliable_git_remote_head(VCSDir)),
    do_exec(Context, 'vcs remote head', [ 'VCSDir' = VCSDir ],
            capture([git, '-C', VCSDir, 'show-ref' ]),
            [], curdir, StdOut),
    member(L, StdOut),
    split_string(L, "\t ", "", [RmtHead, "refs/remotes/origin/HEAD"]).

prolog:message(unreliable_git_remote_head(VCSDir)) -->
    [ "Using unreliable fallback to get git remote head SHA from ~w" - [VCSDir]].

%% git_current_branch(Context, VCSDir, CurBranch) :-
%%     do_exec(Context, 'vcs remote head', [ 'VCSDir' = VCSDir ],
%%             capture([git, '-C', VCSDir, status ]),
%%             [], curdir, StdOut),
%%     member(L, StdOut),
%%     split_string(L, " ", "", ["On", "branch", CurBranch]).

git_build_status_url(URL, Fetch_SHA, StatusURL) :-
    member(host(H), URL),
    sub_string(H, _, _, _, "gitlab"),  %% <--- selector
    git_repo_path(URL, PathParts),
    intercalate(PathParts, "%2F", Project),
    intercalate(["api", "v4", "projects", Project,
                 % "repository", "commits", "master" ],  % n.b. needs correct branch name, does not accept SHA
                 % "commits", Fetch_SHA, "status"],  % used to be "builds" instead of "commits";
                 "repository", "commits", Fetch_SHA, "statuses" ],
                "/", SPath),
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
    (DSts == 0
    -> dgsync(Context, GitTool, Sts)
    ; Sts = DSts
    ).

vctl_push(_Context, Tool, _Args, 1) :-
    print_message(error, unknown_vcs_tool(Tool)).

dgsync(Context, git(VCSDir), Sts) :- dgsync(Context, indir(VCSDir), Sts).
dgsync(Context, git(VCSDir, _), Sts) :- dgsync(Context, indir(VCSDir), Sts).
dgsync(Context, indir(VCSDir), Sts) :-
    (eng:eng(vctl, dgsync, signature, Sig)
    -> Cmd = [ "dgsync", '-s', Sig, VCSDir ]
    ; Cmd = [ "dgsync", VCSDir ]
    ),
    do_exec(Context, 'dgsync', [], [Cmd], [], curdir, Sts).

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
    dgsync(Context, GitTool, GSts),
    vctl_pull(Context, darcs(DarcsDir), Args, DSts),
    sum_list([DSts, GSts], Sts).

vctl_pull(_Context, Tool, _Args, 1) :-
    print_message(error, unknown_vcs_tool(Tool)).

% ----------------------------------------------------------------------
% vctl subproj helpers

% Get the preface to use for printing information about the named subproj
vctl_subproj_preface(Name, Preface) :-
    format(atom(Preface), '#____ ~w :: ', [Name]).

vctl_subproj_context(context(EngDir, TopDir), SubProjDir, context(EngDir, SPDir)) :-
    directory_file_path(TopDir, SubProjDir, SPDir).

vctl_subproj_context_has_engfiles(context(_, SPDir)) :-
    has_engfiles(SPDir, _).

vctl_subproj_local_dir(Name, LclDir) :-
    eng:eng(vctl, subproject, Name, into, LclDir), !.
vctl_subproj_local_dir(Name, LclDir) :-
    format(atom(LclDir), 'subproj/~w', [Name]).

vctl_subcmd(Context, Op, Args, SubSts) :-
    setof((S,D), (eng:key(vctl, subproject, S),
                  vctl_subproj_local_dir(S, D),
                  exists_directory(D)
                 ), SDS),
    !,
    vctl_subcmd_each(Context, Op, Args, SDS, SubSts).
vctl_subcmd(_, _, _, []).  % setof fails if there is nothing found, fall thru here
vctl_subcmd_each(_Context, _Op, _Args, [], [0]).
vctl_subcmd_each(Context, Op, Args, [(S,D)|SDS], [sts(S,Sts)|SubSts]) :-
    vctl_subproj_context(Context, D, SubContext),
    (vctl_subproj_context_has_engfiles(SubContext)
    -> Sts = 0  % will be handled by a separate ingest_engfiles processing run
    ; vcs_tool(SubContext, VCTool),
      call(Op, SubContext, VCTool, Args, Sts)
    ),
    vctl_subcmd_each(Context, Op, Args, SDS, SubSts).

vctl_changes(Context, git(VCSDir)) :-
    do_exec(Context, 'vcs git changes?', [],
            capture([ "git","-C", VCSDir, "status", "-s" ]),
            curdir, [], Out),
    Out \= [].
vctl_changes(Context, git(VCSDir, _)) :-
    do_exec(Context, 'vcs git unpushed?', ['VCSDir' = VCSDir],
            capture([ git, "-C", VCSDir, "rev-list", "@{upstream}..HEAD" ]),
            [], curdir, Out),
    Out \= [].
vctl_changes(Context, darcs(VCSDir)) :-
    catch(do_exec(Context, 'vcs darcs changes?', [],
                  capture(["darcs", "status", "-s", "-q"]),
                  [], VCSDir, Out),
          _, fail),
    Out \= [].
vctl_changes(Context, darcs(VCSDir)) :-
    do_exec(Context, 'vcs darcs unpushed?', [],
            capture([ "darcs", "push", "--dry-run", "-q" ]),
            [], VCSDir, Out),
    Out \= [].
vctl_changes(Context, darcs(VCSDir, _)) :-
    vctl_changes(Context, darcs(VCSDir)).

% Returns remote address: darcsremote(String), gitremote_http(parse_http URL, Auth), gitremote_ssh(String),
% miscremote(String), or rmtNotSpecified.
vctl_subproj_remote_repo(VCTool, Name, Rmt) :-
    eng:eng(vctl, subproject, Name, repo, Repo),
    string_concat("{SIBLING}/", RepoName, Repo),
    vctl_repo_remote_addr(VCTool, RepoRemote),
    vctl_repo_addr_new_repo(RepoRemote, RepoName, Rmt),
    % If the remote is a directory (i.e. another local checkout), see if that
    % exists... otherwise, this fails an another repo setting for this Name is
    % checked.
    vctl_repo_check_remote_exists_if_dir(Rmt),
    !.
vctl_subproj_remote_repo(_, Name, gitremote_ssh(Rmt)) :-
    eng:eng(vctl, subproject, Name, repo, Rmt),
    string_concat("git@", _, Rmt),
    !.
vctl_subproj_remote_repo(VCTool, Name, Rmt) :-
    eng:eng(vctl, subproject, Name, repo, Rmt),
    parse_url(Rmt, URL),
    string_contains(Rmt, "git"),
    !,
    git_rmt_with_auth(VCTool, URL, Rmt).
vctl_subproj_remote_repo(darcs(_), Name, darcsremote(Rmt)) :-
    eng:eng(vctl, subproject, Name, repo, Rmt),
    split_string(Rmt, ":", "", Split),
    length(Split, SL),
    SL > 1,
    !.
vctl_subproj_remote_repo(_VCTool, Name, miscremote(Rmt)) :-
    eng:eng(vctl, subproject, Name, repo, Rmt),
    !.
vctl_subproj_remote_repo(_VCTool, _Name, rmtNotSpecified).

git_rmt_with_auth(git(_, forge(_, Auth)), URL, gitremote(URL, Auth)).
git_rmt_with_auth(_, URL, gitremote(URL, noAuth)).

vctl_repo_remote_addr(git(_, forge(URL, Auth)), gitremote(URL, Auth)) :- !.
vctl_repo_remote_addr(darcs(VCSDir, _), darcsremote(Remote)) :-
    darcs_remote_repo(VCSDir, Remote), !.
vctl_repo_remote_addr(darcs(VCSDir), darcsremote(Remote)) :-
    darcs_remote_repo(VCSDir, Remote), !.
vctl_repo_remote_addr(VCTool, _) :-
    print_message(error, no_repo_remote(VCTool)),
    fail.

vctl_repo_check_remote_exists_if_dir(gitremote(_, _)). % just assumes
vctl_repo_check_remote_exists_if_dir(darcsremote(Remote)) :-
    parse_url(Remote, URL),
    member(host(H), URL),
    H \= '',
    !.  % if remote, assume it exists
vctl_repo_check_remote_exists_if_dir(darcsremote(Remote)) :-
    exists_directory(Remote),
    !.  % local dir, must exist

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
    parse_url(R, URL),
    string_contains(R, "git"),
    !.
vctl_subproj_remote_repo_str(gitremote(URL, _), R) :-
    parse_url(S, URL),
    string_concat("git ", S, R),
    !.
vctl_subproj_remote_repo_str(gitremote_ssh(S), S) :-
    string_contains(S, "git"),
    !.
vctl_subproj_remote_repo_str(gitremote_ssh(S), R) :-
    string_concat("git ", S, R), !.
vctl_subproj_remote_repo_str(miscremote(S), S) :- !.
vctl_subproj_remote_repo_str(Rmt, R) :- format(atom(R), '?? ~w', [Rmt]).

% Get the remote revision.  This may be specified via EQIL or may come from some
% other source (e.g. cabal.project, flake.lock, etc.).
vctl_subproj_remote_rev(Name, Rev) :-
    eng:eng(vctl, subproject, Name, rev, Rev).

% ----------------------------------------------------------------------

vctl_subproj_show(Context, VCTool, (Name, IntoDir), IsPresent) :-
    vctl_subproj_preface(Name, Pfc),
    (exists_context_subdir(Context, IntoDir)
    -> IsPresent = 1,
       ansi_format([bold], '~w~`-t> ~w~78|~n', [ Pfc, IntoDir ])
    ; IsPresent = 0,
      vctl_subproj_remote_repo(VCTool, Name, RmtAddr),
      vctl_subproj_remote_repo_str(RmtAddr, Rmt),
      (vctl_subproj_remote_rev(Name, Rev) ; Rev = ""),
      ansi_format([], "~w~` t~w ~w~78|~n", [Pfc, Rmt, Rev])
    ).

% ----------------------------------------------------------------------

vctl_subproj_clone(Context, VCTool, DepName, CloneSts) :-
    eng:key(vctl, subproject, DepName),
    !,
    vctl_subproj_local_dir(DepName, TgtDir),
    file_directory_name(TgtDir, TgtParentDir),
    ensure_context_subdir(Context, TgtParentDir),
    vctl_subproj_clone_into(Context, VCTool, DepName, TgtDir, CloneSts).
vctl_subproj_clone(context(EngDir, _TopDir), _, DepName,
                   unknown(DepName, unknown_subproject(EngDir, DepName))).

vctl_subproj_clone_into(Context, VCTool, DepName, TgtDir, sts(DepName, 0)) :-
    exists_context_subdir(Context, TgtDir),
    !,
    print_message(warning, clone_tgt_already_exists(TgtDir)),
    % Still perform post-clone operation to keep things synchronized: the subproj
    % might have been checked out manually.
    vctl_post_clone(Context, VCTool, TgtDir).
vctl_subproj_clone_into(Context, VCTool, DepName, TgtDir, sts(DepName, CloneSts)) :-
    vctl_subproj_remote_repo(VCTool, DepName, RmtRepo),
    (vctl_subproj_remote_rev(DepName, Rev), ! ; Rev = head),
    vctl_clone(Context, RmtRepo, Rev, TgtDir, CloneSts),
    (CloneSts == 0
    -> vctl_post_clone(Context, VCTool, TgtDir)
    ; true).

prolog:message(clone_tgt_already_exists(TgtDir)) -->
    [ 'Clone target ~w already exists' - [TgtDir] ].

vctl_post_clone(context(_, TopDir), VCTool, _TgtDir) :-
    eng:eng(vctl, 'post clone', OpStr), !,
    atom_string(Op, OpStr),
    call(Op, VCTool, TopDir).
vctl_post_clone(_, _, _).

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
vctl_clone(context(EngDir, TopDir), gitremote_ssh(URL), head, TgtDir, 0) :-
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = URL ],
            [ 'git clone {RepoAddr} {TgtDir}' ],
            [], TopDir, 0), !.
vctl_clone(context(EngDir, TopDir), gitremote_ssh(URL), Rev, TgtDir, 0) :-
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = URL, 'Ref' = Rev ],
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
    format(atom(X), '--to-hash=~w', [SelVal]),
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo, 'Ref' = X ],
            [ 'darcs clone {Ref} {RepoAddr} {TgtDir}' ],
            [], TopDir, Sts).
darcs_clone(context(EngDir, TopDir), Repo, "patch", SelVal, TgtDir, Sts) :-
    !,
    format(atom(X), '--to-patch=~w', [SelVal]),
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo, 'Ref' = X ],
            [ 'darcs clone {Ref} {RepoAddr} {TgtDir}' ],
            [], TopDir, Sts).
darcs_clone(context(EngDir, TopDir), Repo, "match", head, TgtDir, Sts) :-
    !,
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo ],
            [ 'darcs clone {RepoAddr} {TgtDir}' ],
            [], TopDir, Sts).
darcs_clone(context(EngDir, TopDir), Repo, "match", SelVal, TgtDir, Sts) :-
    !,
    format(atom(X), '--to-match=~w', [SelVal]),
    do_exec(context(EngDir, TopDir), 'vcs clone',
            [ 'TgtDir' = TgtDir, 'RepoAddr' = Repo, 'Ref' = X ],
            [ 'darcs clone {Ref} {RepoAddr} {TgtDir}' ],
            [], TopDir, Sts).
darcs_clone(_, _, Sel, SelVal, _, 1) :-
    print_message(error, unknown_darcs_revtype(Sel, SelVal)).

% ----------------------------------------------------------------------

vctl_subproj_remove(Context, VCTool, DepName, CloneSts) :-
    eng:key(vctl, subproject, DepName),
    !,
    remove_subproj(Context, VCTool, DepName, CloneSts).
vctl_subproj_remove(context(EngDir, _TopDir), _, DepName,
                    unknown(DepName, unknown_subproject(EngDir, DepName))).

remove_subproj(context(EngDir, TopDir), VCTool, DepName, CloneSts) :-
    working_directory(OldDir, TopDir),
    vctl_subproj_local_dir(DepName, TgtDir),
    exists_directory(TgtDir),
    !,
    remove_subproj_if_clean(context(EngDir, TopDir), VCTool, DepName, TgtDir, CloneSts),
    working_directory(_, OldDir).
remove_subproj(_, _, DepName, sts(DepName, 0)) :-
    vctl_subproj_local_dir(DepName, TgtDir),
    print_message(info, subproj_not_cloned(DepName, TgtDir)).

remove_subproj_if_clean(Context, _VCTool, DepName, TgtDir, sts(DepName, 1)) :-
    vctl_subproj_context(Context, TgtDir, SPContext),
    vcs_tool(SPContext, TgtDirVCTool),
    vctl_changes(Context, TgtDirVCTool),
    !,
    print_message(error, subproj_not_clean(DepName, TgtDir)).
remove_subproj_if_clean(Context, VCTool, DepName, TgtDir, sts(DepName, 0)) :-
    vctl_subproj_preface(DepName, Pfc),
    format('~w~w~n', [ Pfc, TgtDir ]),
    delete_directory_and_contents(TgtDir),
    format('Local copy (~w) removed.~n', [TgtDir]),
    vctl_post_clone(Context, VCTool, TgtDir).


% ----------------------------------------------------------------------

prolog:message(unknown_vcs_tool(Tool)) -->
    [ 'Unknown VCS tool: ~w.  Unable to perform action' - [ Tool ] ].
prolog:message(vcs_tool_undefined(TopDir)) -->
    [ 'Unable to determine the VCS tool to use (e.g. git, darcs) in ~w'-[TopDir]
    ].
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
prolog:message(subproj_not_cloned(DepName, TgtDir)) -->
    [ 'Subproject ~w is not currently locally cloned (into ~w)' - [ DepName, TgtDir ]].
prolog:message(subproj_not_clean(DepName, TgtDir)) -->
    [ 'Local changes to subproject ~w in ~w; not removing!' - [ DepName, TgtDir ]].
