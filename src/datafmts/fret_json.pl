% Converts parsed fretish into the on-disk JSON stored format.

:- module(fret_json, [ lando_reqs_to_fret_json/3,
                       fretment_to_fretsemantics/3,
                       replace_template_vars/5
                     ]).

:- use_module(library(apply)).
:- use_module('../englib').
:- use_module('../exprlang').
:- use_module(ltl).
:- use_module(lustre).
:- use_module(frettish).

lando_reqs_to_fret_json(Lando_Reqs, Vars, JSONDict) :-
    % lando_fret:get_semantics_defs(_), % asserts fret_semantics/5 facts used
    % below
    maplist(lando_req_to_fret_json, Lando_Reqs, JSONReqs),
    mk_fret_vars(JSONReqs, Vars, JSONVars),
    JSONDict = _{requirements: JSONReqs, variables: JSONVars}.

lando_req_to_fret_json(Lando_Req, JSONDict) :-
    get_dict(lando_req, Lando_Req, R),
    get_dict(req_id, R, RI),
    get_dict(req_parent_id, R, PI),
    get_dict(req_name, R, RN),
    get_dict(req_desc, R, RD),
    get_dict(req_project, R, RP),
    get_dict(fret_req, R, RF),
    emit_fretish(RF, FT, Ranges),
    fretment_to_fretsemantics(RF, Ranges, FJ),
    JSONDict = _{ '_id': RI, reqid: RN, parent_reqid: PI, project: RP,
                  fulltext: FT,
                  rationale: RD, status: "", comments: "", semantics: FJ }.


% Given a fretment(scope_info(..), condition_info(..), component_info(..),
%                  timing_info(..), response_info(..))
% returns:
%   dictionary of "semantics" portion of FRET JSON for that fretment.
%
fretment_to_fretsemantics(Fretment, Ranges, JSONDict) :-
    Fretment = fretment(scope_info(Scope, _),
                        condition_info(Condition),
                        component_info(Comp),
                        timing_info(Timing, _),
                        response_info(Response)),

    % Ensure all the FRETish, LTL, and Lustre/CoCoSpec language definitions are
    % in effect.
    %% fretish_expr_langdef(Fretish_Expr_Langdef),
    %% define_language(Fretish_Expr_Langdef, _),
    define_ltl_language,
    define_lustre_language,

    % Now "hydrate" the information with all of the additional things that the
    % FRET tool expects to find in the JSON form.

    fretment_vars(scope, Fretment, ScopeVars),
    fretment_vars(condition, Fretment, CondVars),
    fretment_vars(timing, Fretment, TimingVars),
    fretment_vars(response, Fretment, RespVars),
    append([ScopeVars, CondVars, TimingVars, RespVars], AllVars),
    maplist(var_name, AllVars, AllVarNames),
    list_to_set(AllVarNames, Vars),
    SemanticsBase = semantics{ type: "nasa",
                               % The set of variables referenced by this fret
                               % requirement; n.b. referenced by name and not
                               % by the _id.
                               variables: Vars
                             },
    (get_dict(scopeTextRange, Ranges, STR)
    -> (put_dict(Scope, _{scopeTextRange: STR}, ScopeJSON),
        put_dict(SemanticsBase, ScopeJSON, Sem1))
    ; put_dict(SemanticsBase, Scope, Sem1)
    ),

    get_dict(conditionTextRange, Ranges, CndTR),
    put_dict(Condition, _{conditionTextRange: CndTR}, ConditionJSON),
    put_dict(Sem1, ConditionJSON, Sem2),  % TODO: if no condition, this is null and breaks the next

    get_dict(componentTextRange, Ranges, CTR),
    get_dict(component, Comp, CN),
    put_dict(Comp, _{componentTextRange: CTR, component_name: CN}, CompJSON),
    put_dict(Sem2, CompJSON, Sem3),

    get_dict(timingTextRange, Ranges, TTR),
    put_dict(Timing, _{timingTextRange: TTR}, TimingJSON),
    put_dict(Sem3, TimingJSON, Sem4),

    get_dict(responseTextRange, Ranges, RTR),
    put_dict(Response, _{responseTextRange: RTR}, ResponseJSON),
    put_dict(Sem4, ResponseJSON, Sem5),

    req_semantics_defs(Sem5, SemDefs),
    create_variable_description(Sem5, DV),

    get_dict(description, SemDefs, Desc0),
    replace_template_vars(Sem5, "<b><i>", "</i></b>", Desc0, Desc),

    (generate_formulae(SemDefs, Sem5, Sem6)
    -> put_dict(_{ diagramVariables: DV,
                   description: Desc
                 }, Sem6, JSONDict)
    ; print_message(error, no_formula()), fail
    ).

prolog:message(no_formula()) --> [ 'Could not generate formulae for frettish' ].

req_semantics_defs(Inp, Semantics) :-
    get_dict(scope, Inp, S),
    get_dict(type, S, ST),
    get_dict(condition, Inp, C),
    get_dict(timing, Inp, T),
    get_dict(response, Inp, Rsp),
    atom_string(STA, ST),
    atom_string(CA, C),
    atom_string(TA, T),
    atom_string(RA, Rsp),
    lando_fret:fret_semantics(STA, CA, TA, RA, Semantics).
    %% format(atom(DefKeyA), '~w,~w,~w,~w', [ST, C, T, Rsp]),
    %% get_dict(DefKeyA, Defs, Semantics).

% ------------------------------

create_variable_description(Inp, VarDesc):-
    get_dict(scope, Inp, Scope),
    get_dict(type, Scope, ST),
    cvd_st(Inp, ST, VarDesc).

cvd_st(Inp, "null", VarDesc) :-
    !,
    get_dict(condition, Inp, C),
    cvd_cnd(Inp, C, VarDesc).
cvd_st(Inp, _, VarDesc) :-
    get_dict(condition, Inp, C),
    cvd_cnd(Inp, C, Y),
    (cv_field_as(Inp, scope_mode, "M", VD), !,
     format_str(VarDesc, '~w, ~w', [VD, Y])
    ; VarDesc = Y
    ).

cvd_cnd(Inp, "regular", VarDesc) :-
    !,
    get_dict(timing, Inp, T),
    cvd_tim(Inp, T, Y),
    cv_field_as(Inp, regular_condition, "TC", VD),
    format_str(VarDesc, '~w, ~w', [VD, Y]).
cvd_cnd(Inp, "noTrigger", VarDesc) :-
    !,
    get_dict(timing, Inp, T),
    cvd_tim(Inp, T, Y),
    cv_field_as(Inp, regular_condition, "CC", VD),
    format_str(VarDesc, '~w, ~w', [VD, Y]).
cvd_cnd(Inp, _, VarDesc) :-
    get_dict(timing, Inp, T),
    cvd_tim(Inp, T, VarDesc).

cvd_tim(Inp, T, VarDesc) :-
    member(T, ["after", "within", "for"]),
    !,
    get_dict(response, Inp, R),
    cvd_rsp(Inp, R, Y),
    get_dict(duration, Inp, D),
    format_str(VarDesc, 'n = <b><i>~w</i></b>, ~w', [D, Y]).
cvd_tim(Inp, T, VarDesc) :-
    member(T, ["until", "before"]),
    !,
    get_dict(response, Inp, R),
    cvd_rsp(Inp, R, Y),
    cv_field_as(Inp, stop_condition, "SC", VD),
    format_str(VarDesc, '~w, ~w', [VD, Y]).
cvd_tim(Inp, _, VarDesc) :-
    get_dict(response, Inp, R),
    cvd_rsp(Inp, R, VarDesc).

cvd_rsp(Inp, "satisfaction", VarDesc) :-
    !,
    cv_field_as(Inp, post_condition, "Response", VarDesc).
cvd_rsp(Inp, "action", VarDesc) :-
    !,
    get_dict(action, Inp, D),
    format(atom(X), 'Response = <b><i>~w</i></b>', [D]),
    atom_string(X, VarDesc).
cvs_rsp(_, _, "").

cv_field_as(Inp, Fld, Name, Text) :-
    get_dict(Fld, Inp, fretish(D)),
    fretish_expr_langdef(FretishExpr),
    get_dict(language, FretishExpr, FretishExprLang),
    (emit_expr(FretishExprLang, D, DTxt), ! ; DTxt = D),
    format_str(Text, '~w = <b><i>~w</i></b>', [Name, DTxt]).

% --------------------------------------------------

replace_template_vars(Inp, Pre, Post, Text, OutText) :-
    string_chars(Text, Chars),
    phrase(replVars(Inp, Pre, Post, OutChars), Chars, []),
    string_chars(OutText, OutChars).

replVars(Inp, Pre, Post, R) -->
    ['$'], w(W), ['$'],
    { !,
      atom_string(WA, W),
      get_dict(WA, Inp, V),
      var_xlate(V, VTxt),
      format(atom(A), '~w~w~w', [Pre, VTxt, Post]),
      atom_string(A,S)
    },
    replVars(Inp, Pre, Post, T),
    { string_chars(TS, T),
      string_concat(S, TS, R)
    }.
replVars(Inp, Pre, Post, R) -->
    [C],
    replVars(Inp, Pre, Post, T),
    { string_chars(S, [C]), string_concat(S, T, R) }.
replVars(_, _, _, "") --> [].

var_xlate(fretish(V), VTxt) :-
    fretish_expr_langdef(FretishExpr),
    get_dict(language, FretishExpr, FretishExprLang),
    emit_expr(FretishExprLang, V, VTxt),
    !.
var_xlate(V, VTxt) :-
    fretish_expr_langdef(FretishExpr),
    get_dict(language, FretishExpr, FretishExprLang),
    emit_expr(FretishExprLang, V, VTxt),
    !.
var_xlate(V, V).

w(W) --> [C], { wchar(C) }, w_(CS), { string_chars(W, [C|CS]) }.
w_([C|CS]) --> [C], { wchar(C) }, !, w_(CS).
w_([]) --> [].

wchar(C) :- \+ char_type(C, space),
            % Exclude things that might be individual tokens needing to be
            % recognized elsewhere in the grammar.
            \+ member(C, ['(', ')', '.', '!', '?', ':', ',',
                          %% '{', '}', '^', '[', ']', %% XXX?
                          '$',
                          '/']).

% ----------------------------------------------------------------------

mk_fret_vars(JSONReqs, Vars, FretVars) :-
    maplist(mk_fret_var(JSONReqs), Vars, FretVars).

mk_fret_var(JSONReqs, constr(VName, Val, Desc, VDef), FV) :-
    !,
    mk_fret_var(JSONReqs, VDef, BaseVar),
    reqs_using_var(JSONReqs, VName, ReqIDs),
    format(atom(ValStr), '(~w)', [Val]), % needs to be a JSON string
    % see NOTE below for required VarID
    get_dict(proj, VDef, ProjName),
    get_dict(comp, VDef, CompName),
    format_str(VarID, '~w~w~w', [ProjName, CompName, VName]),
    put_dict(_{variable_name: VName,
               assignment: ValStr,
               idType: "Internal",
               description: Desc,
               reqs: ReqIDs,
               '_id': VarID
              }, BaseVar, FV).

mk_fret_var(JSONReqs, V, FV) :-
    get_dict(varname, V, VName),
    get_dict(type, V, VType),
    get_dict(proj, V, ProjName),
    get_dict(comp, V, CompName),
    get_dict(usage, V, Usage),
    get_dict(desc, V, Desc),
    (atom_concat(_, '_τ', VType)
    -> Type = integer
    ; (VType == bool
      -> Type = boolean
      ; Type = VType
      )
    ),
    reqs_using_var(JSONReqs, VName, ReqIDs),
    % NOTE: the following is *REQUIRED* for the VarID, otherwise FRET will not
    % accept the variable information.
    format_str(VarID, '~w~w~w', [ProjName, CompName, VName]),
    FV = _{ project: ProjName,
            component_name: CompName,
            variable_name: VName,
            reqs: ReqIDs,
            dataType: Type,
            idType: Usage,
            moduleName: "",
            description: Desc,
            assignment: "",
            copilotAssignment: "",
            modeRequirement: "",
            modeldoc: false,
            modelComponent: "",
            modeldoc_id: "",
            completed: true,
            '_id': VarID
          }.


reqs_using_var([], _, []).
reqs_using_var([JR|JSONReqs], VName, [ReqID|ReqIDs]) :-
    get_dict(semantics, JR, S),
    get_dict(variables, S, VS),
    member(VName, VS),
    !,
    get_dict('_id', JR, ReqID),
    reqs_using_var(JSONReqs, VName, ReqIDs).
reqs_using_var([_|JSONReqs], VName, ReqIDs) :-
    reqs_using_var(JSONReqs, VName, ReqIDs).

% ----------------------------------------------------------------------
% JSON output conversions

% These two allow exprlang op(..) and term(..) LTL elements to be converted to
% JSON.  Note that these are any exprlang op and term references, which are
% assumed to be LTL.
json:json_write_hook(op(O, T), Strm, _, _) :-
    (emit_ltl(op(O, T), Txt), !
    ; print_message(warning, convert_JSON_failure(op(O, T))), Txt = O
    ),
    format(Strm, '"~w"', [Txt]).
json:json_write_hook(term(E, T), Strm, _, _) :-
    (emit_ltl(term(E, T), Txt), !
    ; print_message(warning, convert_JSON_failure(term(E, T))), Txt = E
    ),
    format(Strm, '"~w"', [Txt]).

json:json_write_hook(ltl(op(O, T)), Strm, _, _) :-
    (emit_ltl(op(O, T), Txt), !
    ; print_message(warning, convert_JSON_failure(op(O, T))), Txt = O
    ),
    format(Strm, '"~w"', [Txt]).
json:json_write_hook(ltl(term(E, T)), Strm, _, _) :-
    (emit_ltl(term(E, T), Txt), !
    ; print_message(warning, convert_JSON_failure(term(E, T))), Txt = E
    ),
    format(Strm, '"~w"', [Txt]).

json:json_write_hook(cocospec(op(O, T)), Strm, _, _) :-
    (emit_CoCoSpec(op(O, T), Txt), !
    ; print_message(warning, convert_JSON_failure(fretish(op(O, T)))),
      Txt = O
    ),
    format(Strm, '"~w"', [Txt]).
json:json_write_hook(cocospec(term(E, T)), Strm, _, _) :-
    (emit_CoCoSpec(term(E, T), Txt), !
    ; print_message(warning, convert_JSON_failure(fretish(term(E, T)))),
      Txt = E
    ),
    format(Strm, '"~w"', [Txt]).

json:json_write_hook(fretish(op(O, T)), Strm, _, _) :-
    fretish_expr_langdef(LD),
    get_dict(language, LD, Language),
    (emit_expr(Language, op(O, T), Txt), !
    ; print_message(warning, convert_JSON_failure(fretish(op(O, T)))),
      Txt = O
    ),
    format(Strm, '"~w"', [Txt]).
json:json_write_hook(fretish(term(E, T)), Strm, _, _) :-
    fretish_expr_langdef(LD),
    get_dict(language, LD, Language),
    (emit_expr(Language, term(E, T), Txt), !
    ; print_message(warning, convert_JSON_failure(fretish(term(E, T)))),
      Txt = E
    ),
    format(Strm, '"~w"', [Txt]).

prolog:message(convert_JSON_failure(E)) -->
    [ 'JSON conversion failed for ~w' - [E]].


% ----------------------------------------------------------------------

generate_formulae(Defs, Inp, Out) :-
    (scope_mode_transform(Defs, Inp, O1)
    ; print_message(error, scope_transform_failed()), fail
    ),
    !,
    (regular_condition_transform(Defs, O1, O2)
    ; print_message(error, condition_transform_failed(regular, O1)), fail
    ),
    !,
    (post_condition_transform(Defs, O2, O3)
    ; print_message(error, condition_transform_failed(post, O2)), fail
    ),
    !,
    (stop_condition_transform(Defs, O3, O4)
    ; print_message(error, condition_transform_failed(stop, O3)), fail
    ),
    !,
    (fetched_ft_pt_update(Defs, O4, O5)
    -> Out = O5
    ; print_message(error, past_time_update_error()), fail
    ).

prolog:message(scope_transform_failed()) --> [ 'Cannot transform scope' ].
prolog:message(condition_transform_failed(Condtype, AST)) -->
    [ 'Cannot transform ~w condition: ~w' - [ Condtype, AST ] ].
prolog:message(past_time_update_error()) -->
    [ 'Cannot update past-time formula' ].

scope_mode_transform(_Defs, Inp, Out) :-
    get_dict(scope, Inp, S),
    get_dict(type, S, ST),
    sc_mo_tr(ST, Inp, Out).
sc_mo_tr(null, I, O) :-
    !,
    put_dict(I, _{ scope_mode_pt: "BAD_PT", scope_mode_ft: "BAD_FT" }, O).
sc_mo_tr(_, I, O) :-
    get_dict(scope_mode, I, fretish(MD)),
    xform_past_temporal_unbounded(MD, MP),
    xform_future_temporal_unbounded(MD, MF),
    put_dict(_{ scope_mode_pt: ltl(MP), scope_mode_ft: ltl(MF) }, I, O).


regular_condition_transform(Defs, Inp, Out) :-
    get_dict(regular_condition, Inp, fretish(RC)), !,
    xform_cond(Defs, Inp, RC, PT, FT, SMVPT, SMVFT),
    put_dict(_{ regular_condition_unexp_pt: PT,
                regular_condition_unexp_ft: FT,
                regular_condition_SMV_pt: SMVPT,
                regular_condition_SMV_ft: SMVFT
              }, Inp, Out).
regular_condition_transform(_, Inp, Inp).

post_condition_transform(Defs, Inp, Out) :-
    get_dict(post_condition, Inp, fretish(PC)), !,
    xform_cond(Defs, Inp, PC, PT, FT, SMVPT, SMVFT),
    put_dict(_{ post_condition_unexp_pt: PT,
                post_condition_unexp_ft: SMVPT,
                post_condition_SMV_pt: FT,
                post_condition_SMV_ft: SMVFT
              }, Inp, Out).
post_condition_transform(_, Inp, Inp).

stop_condition_transform(Defs, Inp, Out) :-
    get_dict(stop_condition, Inp, fretish(SC)),
    !,
    xform_cond(Defs, Inp, SC, PT, FT, SMVPT, SMVFT),
    put_dict(_{ stop_condition_unexp_pt: PT,
                stop_condition_unexp_ft: SMVPT,
                stop_condition_SMV_pt: FT,
                stop_condition_SMV_ft: SMVFT
              }, Inp, Out).
stop_condition_transform(_, Inp, Inp). % if no stop_condition

xform_cond(Defs, Inp, C, PT, FT, SMVPT, SMVFT) :-
    xform_past_temporal(C, CP),
    xform_future_temporal(C, CF),

    get_dict(endpoints, Defs, Endpoints),
    get_dict(left, Endpoints, Left),
    get_dict(right, Endpoints, Right),
    get_dict('SMVptExtleft', Endpoints, SMVLeft),
    %% get_dict('SMVptExtright', Endpoints, SMVRight),
    %% get_dict('SMVftExtleft2', Endpoints, SMVLeftF),
    get_dict('SMVftExtright2', Endpoints, SMVRightF),
    % All the above are strings from the semantics.json file.  Convert them to
    % LTL ABT's, but don't backtrack beyond this point on parse failures (not
    % expecting any!).
    !,
    parse_ltl(Left, LeftLTL),
    parse_ltl(SMVLeft, SMVLeftLTL),
    parse_ltl(Right, RightLTL),
    parse_ltl(SMVRightF, SMVRightLTL),
    !,

    ltl_langdef(LTLLang),
    get_dict(language, LTLLang, LTL),

    get_dict(scope_mode_pt, Inp, SMPT),
    subst_term(LTL, '$Left$', LeftLTL, CP, CP1),
    subst_term(LTL, '$scope_mode$', SMPT, CP1, PT),
    subst_term(LTL, '$Left$', SMVLeftLTL, CP, CPS1),
    subst_term(LTL, '$scope_mode$', SMPT, CPS1, SMVPT),

    get_dict(scope_mode_ft, Inp, SMFT),
    subst_term(LTL, '$Right$', RightLTL, CF, CF1),
    subst_term(LTL, '$scope_mode$', SMFT, CF1, FT),
    subst_term(LTL, '$Right$', SMVRightLTL, CF, CFS1),
    subst_term(LTL, '$scope_mode$', SMFT, CFS1, SMVFT).

tmpl_final(Inp, SrcFld, ForTemplate, InpLTL, OutLTL) :-
    ltl_langdef(LTLLang),
    get_dict(language, LTLLang, Language),
    (get_dict(SrcFld, Inp, S)
    ; (format(atom(B), '!No_~w!', [SrcFld]), S = term(ident(B), bool))
    ),
    !,  % subst failure should not have retries here
    subst_term(Language, ForTemplate, S, InpLTL, OutLTL).

tmpl_intermediate(SrcFld, IntermField, InpLTL, OutLTL) :-
    ltl_langdef(LTLLang),
    get_dict(language, LTLLang, Language),
    format(atom(T), '$~w$', [SrcFld]),
    !,  % subst failure should not have retries here
    subst_term(Language, IntermField, term(ident(T), bool), InpLTL, OutLTL).

tmplsubs_ltl(Inp, SrcFld, TgtTmpl, InpILTL, InpFLTL, OutILTL, OutFLTL) :-
    tmpl_final(Inp, SrcFld, TgtTmpl, InpFLTL, OutFLTL),
    tmpl_intermediate(SrcFld, TgtTmpl, InpILTL, OutILTL).

tmplsubs_ltl_seq(_, [], ILTL, FLTL, ILTL, FLTL).
tmplsubs_ltl_seq(Inp, [(SrcFld, TgtTempl)|Flds], InpILTL, InpFLTL, OutILTL, OutFLTL) :-
    !,  % no backtracking on failures after this point
    tmplsubs_ltl(Inp, SrcFld, TgtTempl, InpILTL, InpFLTL, ILTL2, FLTL2),
    !,  % no backtracking on failures after this point
    tmplsubs_ltl_seq(Inp, Flds, ILTL2, FLTL2, OutILTL, OutFLTL).

fetched_ft_pt_update(Defs, Inp, Out) :-
    % n.b. do not change the order of substitutions here
    get_dict(ft, Defs, FT),
    parse_ltl(FT, FTLTL),
    tmplsubs_ltl_seq(Inp, [ (regular_condition_unexp_ft, '$regular_condition$'),
                            (post_condition_unexp_ft, '$post_condition$'),
                            (stop_condition_unexp_ft, '$stop_condition$'),
                            (scope_mode_ft, '$scope_mode$')
                          ],
                     FTLTL, FTLTL, FTF, TFT),

    get_dict(pt, Defs, PT),
    parse_ltl(PT, PTLTL),
    tmplsubs_ltl_seq(Inp, [ (regular_condition_unexp_pt, '$regular_condition$'),
                            (post_condition_unexp_pt, '$post_condition$'),
                            (stop_condition_unexp_pt, '$stop_condition$'),
                            (scope_mode_pt, '$scope_mode$')
                          ],
                     PTLTL, PTLTL, PTF, TPT),

    %% ----- this is the one that matters for kind2 ----------
    get_dict(ptExpanded, Defs, PTE),
    parse_ltl(PTE, PTELTL),
    tmplsubs_ltl_seq(Inp, [ (regular_condition_SMV_pt, '$regular_condition$'),
                            (post_condition_SMV_pt, '$post_condition$'),
                            (stop_condition_SMV_pt, '$stop_condition$'),
                            (scope_mode_pt, '$scope_mode$')
                          ],
                     PTELTL, PTELTL, PTEF, PASTRaw),
    xform_past_optimize(PASTRaw, PAST),

    get_dict(ftExpanded, Defs, FTE),
    parse_ltl(FTE, FTELTL),
    tmplsubs_ltl_seq(Inp, [ (regular_condition_SMV_ft, '$regular_condition$'),
                            (post_condition_SMV_ft, '$post_condition$'),
                            (stop_condition_SMV_ft, '$stop_condition$'),
                            (scope_mode_ft, '$scope_mode$')
                          ],
                     FTELTL, FTELTL, FTEF, FSMV),
    xform_future_optimize(FSMV, FOPT),

    get_dict(ftInfAUExpanded, Defs, FAU),
    parse_ltl(FAU, FAULTL),
    tmplsubs_ltl_seq(Inp, [ (regular_condition_SMV_ft, '$regular_condition$'),
                            (post_condition_SMV_ft, '$post_condition$'),
                            (stop_condition_SMV_ft, '$stop_condition$'),
                            (scope_mode_ft, '$scope_mode$')
                          ],
                     FAULTL, FAULTL, FAUF, TFAU),
    last_is_FALSE(TFAU, FAUSMV),
    xform_future_optimize(FAUSMV, FAUOPT),

    % XXX: if constants.generateBetweenSemantics is set, more should be added
    % here, see SemanticsAnalyzer.js

    % n.b. the original form used two subsitutions: one step substituted the
    % plain termplates like $regular_condition$ with a different template
    % customized to the future/past unexp/exp forms, and then those templates
    % were replaced with the corresponding entry as set by the
    % xxx_condition_transform operations.  This was recorded by making the
    % _fetched version the result of the first transform and the variable without
    % _fetched is the result of the second transform.
    %
    % In the new ABT-style handling, the transformation is more direct, so
    % there's some duplication to get the intermediate forms to maintain fidelity
    % with original FRET.

    put_dict(_{ ft_fetched: FTF,
                ft: TFT,
                pt_fetched: PTF,
                pt: TPT,
                ptExpanded_fetched: PTEF,
                ptExpanded: ltl(PAST), % rewritten
                'CoCoSpecCode': cocospec(PAST),
                ftExpanded_fetched: FTEF,
                ftExpandedUnoptimized: FSMV,
                ftExpanded: FOPT,
                ftInfAUExpanded_fetched: FAUF,
                ftInfAUExpanded: FAUOPT
              }, Inp, Out).

last_is_FALSE(I, O) :-
    ltl_langdef(LTLLang),
    get_dict(language, LTLLang, LTL),
    subst_term(LTL, 'LAST', term(lit(false), bool), I, O).

% --------------------

xform_past_temporal_unbounded(AST, O) :-
    ltl_langdef(LTLLang),
    get_dict(language, LTLLang, LTL),
    fmap_abt(LTL, fret_json:xptu, AST, O),
    !.

xform_future_temporal_unbounded(AST, O) :-
    ltl_langdef(LTLLang),
    get_dict(language, LTLLang, LTL),
    fmap_abt(LTL, fret_json:xftu, AST, O),
    !.

xform_past_temporal(AST, O) :-
    ltl_langdef(LTLLang),
    get_dict(language, LTLLang, LTL),
    fmap_abt(LTL, fret_json:xpt, AST, O),
    !.

xform_future_temporal(AST, O) :-
    ltl_langdef(LTLLang),
    get_dict(language, LTLLang, LTL),
    fmap_abt(LTL, fret_json:xft, AST, O),
    !.


% fret-electron/support/xform.js pastTemporalConditionsNoBounds
xptu(op(persisted(Dur, Cond), bool),
     op(ltlH_bound(op(range_max_incl(Dur), range), Cond), bool)).
xptu(op(persisted(Start, Dur, Cond), bool),
    op(ltlH_bound(op(range_min_max(Start, Dur), range), Cond), bool)).
xptu(op(occurred(Dur, Cond), bool),
    op(ltlO_bound(op(range_max_incl(Dur), range), Cond), bool)).
xptu(op(occurred(Start, Dur, Cond), bool),
    op(ltlO_bound(op(range_min_max(Start, Dur), range), Cond), bool)).
xptu(op(prevOcc(P,Q), bool),
     op(ltlS(op(ltlY(op(not(P), bool)), bool),
             op(and(P, Q), bool)), bool)).
% user specification of future terms is invalid for a past-time formula
xptu(op(persists(_, _), bool), R) :- impossible_xform(R).
xptu(op(persists(_, _, _), bool), R) :- impossible_xform(R).
xptu(op(occurs(_, _), bool), R) :- impossible_xform(R).
xptu(op(occurs(_, _, _), bool), R) :- impossible_xform(R).
xptu(op(nextOcc(_, _), bool), R) :- impossible_xform(R).
xptu(I, I).

% fret-electron/support/xform.js futureTemporalConditionsNoBounds
xftu(op(persists(Dur, Cond), bool),
     op(ltlG_bound(op(range_max_incl(Dur), range), Cond), bool)).
xftu(op(persists(Start, Dur, Cond), bool),
     op(ltlG_bound(op(range_min_max(Start, Dur), range), Cond), bool)).
xftu(op(occurs(Dur, Cond), bool),
     op(ltlF_bound(op(range_max_incl(Dur), range), Cond), bool)).
xftu(op(occurs(Start, Dur, Cond), bool),
     op(ltlF_bound(op(range_min_max(Start, Dur), range), Cond), bool)).
xftu(op(nextOcc(P, Q), bool),
     op(ltlU(op(ltlX(op(not(P), bool)), bool),
             op(and(P, Q), bool)), bool)).
xftu(op(persisted(_, _), bool), R) :- impossible_xform(R).
xftu(op(persisted(_, _, _), bool), R) :- impossible_xform(R).
xftu(op(occurred(_, _), bool), R) :- impossible_xform(R).
xftu(op(occurred(_, _, _), bool), R) :- impossible_xform(R).
xftu(op(prevOcc(_, _), bool), R) :- impossible_xform(R).
xftu(I, I).


% fret-electron/support/xform.js pastTemporalConditions
xpt(term(ident('FTP'), bool), op(ltlZ(term(lit(false), bool)), bool)).
xpt(op(persisted(Dur, Cond), bool),
    op(and(op(ltlH_bound(op(range_max_incl(Dur), range), Cond), bool),
           op(ltlH_bound(op(range_max(Dur), range),
                         op(not(term(ident('$Left$'), bool)), bool)), bool)),
       bool)).
xpt(op(persisted(Start, Dur, Cond), bool),
    op(and(op(ltlH_bound(op(range_min_max(Start, Dur), range), Cond), bool),
           op(ltlH_bound(op(range_max(Dur), range),
                         op(not(term(ident('$Left$'), bool)), bool)), bool)),
       bool)).
xpt(op(occurred(Dur, Cond), bool),
    op(and(op(ltlS(op(not(term(ident('$Left$'), bool)), bool),
                   Cond), bool),
           op(ltlO_bound(op(range_max_incl(Dur), range), Cond), bool)),
       bool)).
xpt(op(occurred(Start, Dur, Cond), bool),
    op(ltlS_bound(op(range_min_max(Start, Dur), range),
                  op(not(term(ident('$Left$'), bool)), bool),
                  Cond),
       bool)).
xpt(op(prevOcc(P,Q), bool),
    op(or(term(ident('$Left$'), bool),
          op(ltlY(op(implies(op(ltlS(op(and(op(not(term(ident('$Left$'), bool)), bool),
                                            op(not(P), bool)),
                                        bool),
                                     P),
                                bool),
                             op(ltlS(op(and(op(not(term(ident('$Left$'), bool)), bool),
                                            op(not(P), bool)),
                                        bool),
                                     op(and(P, Q), bool)),
                                bool)),
                     bool)),
             bool)),
       bool)).
xpt(op(preBool(Init,P), bool),
    op(or(op(and(op(ltlZ(term(lit(false), bool)), bool),
                 Init), bool),
          op(and(op(ltlY(term(lit(true), bool)), bool),
                 op(ltlY(P), bool)), bool)), bool)).
xpt(op(persists(_, _), bool), R) :- impossible_xform(R).
xpt(op(persists(_, _, _), bool), R) :- impossible_xform(R).
xpt(op(occurs(_, _), bool), R) :- impossible_xform(R).
xpt(op(occurs(_, _, _), bool), R) :- impossible_xform(R).
xpt(op(nextOcc(_, _), bool), R) :- impossible_xform(R).
xpt(I, I).

% fret-electron/support/xform.js futureTemporalConditions
xft(op(persists(N, P), bool),
    op(and(op(ltlG_bound(op(range_max_incl(N), range), P), bool),
           op(ltlG_bound(op(range_max(N), range),
                         op(not(term(ident('$Right$'), bool)), bool)), bool)), bool)).
xft(op(persists(M, N, P), bool),
    op(and(op(ltlG(op(range_min_max(M,N), range), P), bool),
           op(ltlG_bound(op(range_max(N), range),
                         op(not(term(ident('$Right$'), bool)), bool)), bool)), bool)).
xft(op(occurs(N, P), bool),
    op(and(op(ltlU(op(not(term(ident('$Right$'), bool)), bool), P), bool),
           op(ltlF_bound(op(range_max_incl(N), range), P), bool)), bool)).
xft(op(occurs(M, N, P), bool),
    op(ltlU_bound(op(range_min_max(M, N), rnage),
                  op(not(term(ident('$Right$'), bool)), bool),
                  P), bool)).
xft(op(nextOcc(P, Q), bool),
    op(or(term(ident('$Right$'), bool),
          op(ltlX(op(implies(op(ltlU(op(and(op(not(P), bool),
                                            op(not(term(ident('$Right$'), bool)),
                                               bool)),
                                        bool)), bool),
                             op(ltlU(op(and(op(not(P), bool),
                                            op(not(term(ident('$Right$'), bool)), bool)),
                                        bool),
                                     op(and(P, Q), bool)),
                                bool)),
                     bool)),
             bool)),
       bool)).
xft(op(persisted(_, _), bool), R) :- impossible_xform(R).
xft(op(persisted(_, _, _), bool), R) :- impossible_xform(R).
xft(op(occurred(_, _), bool), R) :- impossible_xform(R).
xft(op(occurred(_, _, _), bool), R) :- impossible_xform(R).
xft(op(prevOcc(_, _), bool), R) :- impossible_xform(R).
xft(I, I).

impossible_xform(op(and(term(lit(false), bool),
                        op(and(term(lit(false), bool),
                               op(and(term(lit(false), bool),
                                      term(lit(false), bool)),
                                  bool)),
                           bool)),
                    bool)).

xform_past_optimize(I, I). % provided/returns AST; already done by ltl_parse

xform_future_optimize(I, I).  % TODO xform.transform(X, xform.optimizeFT)
