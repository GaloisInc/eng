:- module(fret_kind2, [ fret_kind2/3
                      ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module('src/datafmts/lando').
:- use_module('englib').
:- use_module(lando_fret).


%% ----------------------------------------------------------------------

fret_kind2(FretReqs, FretMents, Kind2Comps) :-
    get_dict(requirements, FretMents, Reqs),
    !,
    connected_components(Reqs, FretReqs, 0, CComps),
    fret_to_kind2(CComps, Kind2Comps).

fret_to_kind2([], []).
fret_to_kind2([comp(N, CompName, Reqs)|CCs], [K2|K2s]) :-
    fret_to_kind2(CCs, K2s),
    reqs_to_kind2(Reqs, Kind2),
    K2 = _{ compNum: N,
            compName: CompName,
            kind2: Kind2
          }.

%% ----------------------------------------------------------------------

connected_components([], _, _, []).
connected_components([R|Reqs], FRInfo, N, [CC|CComps]) :-
    req_info(FRInfo, R, RInfo),
    req_resp_vars(RInfo, Vars),
    ccomp(FRInfo, N, R, Vars, Reqs, CC, RemReqs),
    succ(N, M),
    connected_components(RemReqs, FRInfo, M, CComps).

ccomp(_, N, Req, _, [], comp(N, CName, [Req]), []) :-
    get_dict(semantics, Req, ReqSemantics),
    get_dict(component_name, ReqSemantics, CName).
ccomp(FRInfo, N, Req, RespVars, [R|RS], comp(N, Name, [R|CompReqs]), RemReqs) :-
    get_dict(semantics, Req, ReqSemantics),
    get_dict(semantics, R, RSemantics),
    get_dict(component_name, ReqSemantics, CName),
    get_dict(component_name, RSemantics, CName),
    req_info(FRInfo, R, RInfo),
    req_resp_vars(RInfo, Vars),
    member(V, Vars),
    member(V, RespVars),
    % same component name, and overlap between R and Req response vars: R is in
    % this component
    !,
    append(Vars, RespVars, TTLVars),
    ccomp(FRInfo, N, Req, TTLVars, RS, comp(N, Name, CompReqs), RemReqs).
ccomp(FRInfo, N, Req, RespVars, [R|RS], CC, [R|RemReqs]) :-
    ccomp(FRInfo, N, Req, RespVars, RS, CC, RemReqs).

req_info([RI|_], R, RI) :-
    get_dict(requirement, RI, RIR),
    get_dict(reqid, R, RID),
    get_dict(reqid, RIR, RID).
req_info([_|RIS], R, RI) :-
    req_info(RIS, R, RI).

req_resp_vars(RI, RespVars) :-
    get_dict(fretment, RI, fretment(_, _, _, _, response_info(_, RespVars))).

%% ----------------------------------------------------------------------

reqs_to_kind2(Reqs, Kind2) :-
    length(Reqs, NReqs),
    format(atom(Kind2), 'this should be some LUSTRE for ~w reqs~n', [NReqs]).
