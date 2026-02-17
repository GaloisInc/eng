:- module(gitforge, [ git_remote_url/2,
                      git_repo_AUTH/2,
                      git_repo_path/2
                    ]).

:- use_module(library(strings)).
:- use_module(library(url)).
:- use_module('../englib').


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

prolog:message(using_pat(H)) --> [ "Using PAT to access ~w" - [ H ] ].

remove_blanks([""|E], R) :- remove_blanks(E, R), !.
remove_blanks([], []).
remove_blanks([E|ES], [E|R]) :- remove_blanks(ES, R).
