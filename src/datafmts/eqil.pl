:- module(eqil, [ parse_eng_eqil/3,
                  normalize_eqil/2,
                  assert_eqil/1,
                  emit_eqil/2
                ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module('../englib').

%% Parses the specified EQIL file contents (the FName is for error reporting only).
%%
%% The result is a list of `eqil(Keys, Vals)` entries.
%%
%% Keys is a list of `key(Indent, "KEYNAME")`.  Earlier entries in the list are
%% higher-level keys.
%%
%% Vals is a list of `val(Indent, "VALUE")`.  If the Indent of the first `val` is
%% 0 and the value is not an empty string, then this value appears on the same
%% line as the key specification.  Blank lines are represented by `val(0, "")`
%% and can appear anywhere in the value list, including the beginning or end
%% (i.e. trailing blank lines for this value).
%%
%% Although a value may be its own EQIL at the sub-level, that information will
%% also appear simply as a multi-line value for the higher level key.
%%
%% The one edge case is that a key cannot have both a value and sub-keys. If
%% there is an input specification like this, the higher level key will contain
%% all subkeys/subvalues as part of the value, and emitting the EQIL will drop
%% the key portion.

parse_eng_eqil(FName, FContents, Result) :-
    string_codes(FContents, Chars),
    phrase(eqil(Result), Chars, Leftover), !,
    show_warnings(FName, Leftover).


%% Converts the results of parse_eng_eqil into asserted facts in the "eng" model
%% area of the current environment.  This creates and eng:key(key1, key, key3) nd
%% eng:val(key1, key2, key3, val) for each sequence of keys and their value.
%% Note that the values have initial and trailing blanks removed (but not
%% internal blanks).

assert_eqil([]).
assert_eqil([eqil(Keys, [])|CCS]) :-
    keyseq(Keys, Keychain), !, assert_eng(Keychain), assert_eqil(CCS).
assert_eqil([eqil(Keys, [""])|CCS]) :-
    keyseq(Keys, Keychain), !, assert_eng(Keychain), assert_eqil(CCS).
assert_eqil([eqil(Keys, Val)|CCS]) :-
    keyseq(Keys, Keychain),
    vals_as_val(Val, V),
    assert_eng(Keychain, V),
    assert_eqil(CCS).

show_warnings(_, []).
show_warnings(In, Unparsed) :-
    \+ Unparsed = [],
    string_chars(S, Unparsed),
    print_message(warning, unparsed(In, S)).

prolog:message(unparsed(In, Str)) -->
    { string_length(Str, L) },
    [ 'Unparsed ~w trailing text in ~w: ~w' - [ L, In, Str ] ].


%% ----------------------------------------------------------------------

eqil(KeyVals) --> sequence(empty_line, _),
                  eqil_keyvals(norm, [], KeyVals, _Remainder),
                  !,
                  blanks.
eqil([]) --> blanks.


eqil_keyvals(norm, Active, FKVs, Rem) -->   % key =
    indented(N), parse_key(N, K), parse_val(0, val_(0, "", _)),
    !,
    { update_active_new_kv([], Active, N, K, no(val), NextActive, AKVs) },
    eqil_keyvals(norm, NextActive, KVs, Rem),
    { append(AKVs, KVs, FKVs) }.
eqil_keyvals(valblock(VN), Active, FKVs, Rem) -->   % key =
    indented(N), parse_key(N, K), { N =< VN }, parse_val(0, val_(0, "", _)),
    !,
    { update_active_new_kv([], Active, N, K, no(val), NextActive, AKVs) },
    eqil_keyvals(norm, NextActive, KVs, Rem),
    { append(AKVs, KVs, FKVs) }.

eqil_keyvals(norm, Active, FKVs, Rem) -->   % key = value
    indented(N), parse_key(N, K), parse_val(0, V),
    !,
    inline_key_value(norm, Active, N, K, V, FKVs, Rem).
eqil_keyvals(valblock(VN), Active, FKVs, Rem) -->   % key = block value
    indented(N), parse_key(N, K), {N =< VN}, parse_val(0, V),
    !,
    inline_key_value(norm, Active, N, K, V, FKVs, Rem).

eqil_keyvals(Mode, Actives, KVs, Rem) -->   % value
    indented(N), parse_val(N, V),
    { active_alignment(Actives, N, indented) },
    !,
    { update_active_val(Actives, N, V, NextActive) },
    eqil_keyvals(Mode, NextActive, KVs, Rem).

eqil_keyvals(Mode, Active, KVs, Rem) -->   % blank line
    indented(N), parse_val(N, val_(N, "", C)),
    !,
    { update_active_val(Active, N, val_(N, "", C), NextActive) },
    eqil_keyvals(Mode, NextActive, KVs, Rem).

eqil_keyvals(_Mode, Actives, KVs, Rem) -->   % implicit key (equidented or dedented)
    indented(N), parse_val(N, val_(N, V, C)),
    { \+ active_alignment(Actives, N, indented) },
    !,
    { update_active_new_kv([], Actives, N, key_(N, V, C), no(val), NextActive, AKVs) },
    eqil_keyvals(norm, NextActive, SKVs, Rem),
    { append(AKVs, SKVs, KVs) }.

eqil_keyvals(_Mode, Active, KVs, "") -->   % end
    blanks,
    { finish_eqil(Active, KVs) }.

finish_eqil([A0|AS], KVs) :-
    finish_eqil(AS, NextKVs),
    active_parts(A0, AN, AK, _, AV),
    trim_trailing_blanks(AV, AVTrimmed),
    add_eqil(AS, [key(AN, AK)], AVTrimmed, NextKVs, KVs).
finish_eqil([], []).

inline_key_value(_Mode, Active, N, K, val_(_, "|", _), FKVs, Rem) -->
    { !,
      update_active_new_kv([], Active, N, K, no(val), NextActive, AKVs)
    },
    eqil_keyvals(valblock(N), NextActive, KVs, Rem),
    { append(AKVs, KVs, FKVs) }.
inline_key_value(_Mode, Active, N, K, V, FKVs, Rem) -->
    { update_active_new_kv([], Active, N, K, V, NextActive, AKVs) },
    eqil_keyvals(norm, NextActive, KVs, Rem),
    { append(AKVs, KVs, FKVs) }.

trim_trailing_blanks([val_(_,"",_)|Vals], TrimmedVals) :-
    !, trim_trailing_blanks(Vals, TrimmedVals).
trim_trailing_blanks(Vals, Vals).

parse_key(N, key_(N, Key, RCnt)) -->
    string_without("\n=", KeySpc), "=",
    {string_codes_rtrim(KeySpc, KeyC, RCnt), string_codes(Key, KeyC) }.

% Parses the value from an input stream.  The result is a val_(N, V, Y) where N
% is the indentation of the value, V is the value itself, and Y is the line
% number.  The V is a single line of input not containing any tabs or carriage
% returns, terminated by a carriage return (not added to the value)
parse_val(N, val_(N, V, Y)) --> " ", !,
                                parse_val(N, val_(N, V, X)), { succ(X, Y) }.
parse_val(N, val_(N, V, Y)) --> ['\t'], !,  % tabs are treated as single spaces
                                parse_val(N, val_(N, V, X)), { succ(X, Y) }.
parse_val(N, val_(N, V, 0)) --> string_without("\t\n", VC), "\n", !,
                                { string_codes(V, VC) }.
parse_val(N, val_(N, V, 0)) --> remainder(VC), { \+ length(VC, 0), string_codes(V, VC) }.


%% ---- Helpers --------------------------------------------------------

% Update the Active stack with the new key+value line
update_active_new_kv(KVs, Actives, N, K, no(val),
                     [active(K, []),active(key_(LN, LV, 0), [KV])|UAS], KVs) :-
    active_alignment(Actives, N, indented),
    active_value_alignment(Actives, N, indented),
    active_last_value(Actives, LN, LV), !,
    % This new line is indented past the value on the previous line, so the
    % previous line value should become a key as well, and then add this new line
    % to the top of the active stack,
    join_key_val(K, no(val), KV),
    active_add_val(KV, Actives, UAS).
update_active_new_kv(KVs, Actives, N, K, no(val), [active(K, [])|UAS], KVs) :-
    active_alignment(Actives, N, indented), !,
    % This new line is indented, so add it to the top of the active stack, and
    % add the current key+val (i.e. the current line) to all the values on the
    % stack to build up their value element.
    join_key_val(K, no(val), KV),
    active_add_val(KV, Actives, UAS).
update_active_new_kv(KVs, Actives, N, K, V,
                     [active(K, [V]),active(key_(LN, LV, 0), [KV])|UAS], KVs) :-
    active_alignment(Actives, N, indented),
    active_value_alignment(Actives, N, indented),
    active_last_value(Actives, LN, LV), !,
    % This new line is indented past the value on the previous line, so the
    % previous line value should become a key as well, then add this line to the
    % top of the active stack, and add the current key+val (i.e. the current
    % line) to all the values on the stack to build up their value element.
    join_key_val(K, V, KV),
    active_add_val(KV, Actives, UAS).
update_active_new_kv(KVs, Actives, N, K, V, [active(K, [V])|UAS], KVs) :-
    active_alignment(Actives, N, indented), !,
    % This new line is indented, so add it to the top of the active stack, and
    % add the current key+val (i.e. the current line) to all the values on the
    % stack to build up their value element.
    join_key_val(K, V, KV),
    active_add_val(KV, Actives, UAS).
update_active_new_kv(KVs, [A0|AS], N, K, V, UpdActive, RKVs) :-
    % This new line is equidented or dedented.  This means it starts a new
    % key, so the current key at the top of the Active stack should be popped and
    % added to the outputs, then recursively compare this new line to the next
    % Active stack element.
    active_parts(A0, AN, AK, _, AV), !,
    update_active_new_kv(KVs, AS, N, K, V, UpdActive, UKVs),
    add_eqil(AS, [key(AN, AK)], AV, UKVs, RKVs).
% empty Actives stack, push this element
update_active_new_kv(KVs, [], _, K, no(val), [active(K, [])], KVs).
update_active_new_kv(KVs, [], _, K, V, [active(K, [V])], KVs).


% Update the Active stack with a value-only line.  This could still be an
% implicit key as well, depending on the indentation relative to the last value.
update_active_val(Active, N, V, UpdActive) :-
    active_value_alignment(Active, N, equidented), !,
    % Same indentation as current active, so add this to the entire active
    % stack's value stack.
    active_add_val(V, Active, UpdActive).
update_active_val(Active, N, val_(N, "", C), UpdActive) :-
    % Blank, so it cannot be a key, just a value extension
    active_add_val(val_(N, "", C), Active, UpdActive).
update_active_val(Active, N, V, UpdActive) :-
    active_value_alignment(Active, N, indented),
    active_last_value(Active, LN, LV), !,
    % Increased indentation means the previous value (top of the value stack of
    % the top of the Active stack) is now effectively a key as well.
    %
    % Also, as usual, add this value to the entire remaining active stack's value
    % stack.
    active_add_val(V, Active, RemActive),
    UpdActive = [active(key_(LN, LV, 0), [V])|RemActive].
update_active_val(Active, N, V, UpdActive) :-
    active_value_alignment(Active, N, indented), !,
    % Unlikely: determined to be indented relative to the last value, but then
    % could not extract the last value.  Just add this value to the active stack
    % and continue.
    active_add_val(V, Active, UpdActive).
update_active_val(Active, N, V, UpdActive) :-
    active_value_alignment(Active, N, dedented), !,
    % This value is dedented relative to the last value. This is a trailing
    % effect: the previous value should have been promoted to a key, so it should
    % have been handled by update_active_new_kv.  Thus, we simply need to add the
    % value to the active stack here.
    active_add_val(V, Active, UpdActive).
update_active_val(_Active, _N, _V, _UpdActive).

% Reconstruct the original line from the key and value.
join_key_val(key_(N, K, 0), no(val), val_(N, K, 0)) :- !.
join_key_val(key_(N, K, T), no(val), val_(N, NV, 0)) :-
    string_rpad(K, " ", T, KP),
    string_concat(KP, "=", NV).
join_key_val(key_(N, K, T), val_(_,V,C), NV) :-
    key_and_val(N, K, T, V, C, NV).
key_and_val(N, K, T, V, C, R) :- string_rpad(K, " ", T, KP),
                                 string_concat(KP, "=", KE),
                                 and_val(N, KE, V, C, R).
and_val(N, KE, V, C, val_(N, KEV, 0)) :- string_rpad(KE, " ", C, KEP),
                                         string_concat(KEP, V, KEV).


% Add a new eqil output to the output collection
add_eqil([], K, V, KVs, [eqil(K, RV)|KVs]) :-
    eqil_val_from_active(V, RV).
add_eqil([A0|Actives], K, V, KVs, RKVs) :-
    active_parts(A0, AN, AK, _, _AV),
    add_eqil(Actives, [key(AN, AK)|K], V, KVs, RKVs).
eqil_val_from_active([], []).
eqil_val_from_active([val_(N,V,_)|VS], RV) :-
    eqil_val_from_active(VS, SVS),
    append(SVS, [val(N,V)], RV).

% Split an Active entry into its constituent parts.
active_parts(active(key_(N, Key, T), Vals), N, Key, T, Vals).

% Determine how far the current line is indented.
indented(N) --> " ", indented(M), !, {succ(M, N)}.
indented(N) --> "\t",
                { format(user_error,
                         "Warning, converting tab to space for indentation",
                         []) },
                indented(M), {succ(M, N)}.
indented(0) --> [].

% Determine the alignment of the current line (N) relative to the Active
% (previous) line.
active_alignment([], _, dedented).
active_alignment([Active0|_], N, indented) :-
    active_parts(Active0, KN, _, _, _), N > KN, !.
active_alignment([Active0|_], N, equidented) :-
    active_parts(Active0, KN, _, _, _), N == KN, !.
active_alignment([_|_], _, dedented).


% Determine the alignment of the current value line (N) relative to the
% preceeding value line (the last/top entry on the value stack for the active
% entry at the top of the Active stack).  Assumes that this is a value that is
% indented relative to the key of the top entry of the Active stack.
%
% Special case:
%
%     key = value first line
%           ^-- indentation amount (= length of key + separators)
%
active_value_alignment([], _, equidented).
active_value_alignment([active(_, [])|_], _, indented).
active_value_alignment([Active0|ActiveR], N, Alignment) :-
    Active0 = active(_, [val_(0, "", _)|_]), !,
    % blank value line, ignore it
    active_value_alignment(ActiveR, N, Alignment).
active_value_alignment([Active0|_], N, Alignment) :-
    Active0 = active(key_(KN, K, KT), [val_(0, _, C)|_]), !,
    % 0 index for a non-blank value indicates it is on the same line as the key
    % (or else it's a top level implicit key itself but key alignment has already
    % been checked).
    string_length(K, KL), sum_list([KN, KL, KT, 1, C], VI),
    ( N > VI, Alignment = indented
    ; N == VI, Alignment = equidented
    ; N < VI, Alignment = dedented
    ).
active_value_alignment([Active0|_], N, indented) :-
    Active0 = active(_, [val_(VN, _, _)|_]), N > VN, !.
active_value_alignment([Active0|_], N, equidented) :-
    Active0 = active(_, [val_(VN, _, _)|_]), N == VN, !.
active_value_alignment([_|_], _, dedented).


% Get the "last" value (the value at the top of the value stack at the top of the
% active stack.  Predicate fails if either stack is empty, or if the last value
% is blank.
active_last_value([active(_, [val_(N, V, _)|_])|_], N, V) :- N > 0.


% Add a new value line to each stack entry
active_add_val(_, [], []).
active_add_val(V, [active(K, VS)|AS], [active(K, [V|VS])|AVS]) :-
    active_add_val(V, AS, AVS).


empty_line(empty) --> whites, "\n".

%% ---- eqil results helpers ----------------------------------------------

% Extract the sequence of key name atoms, discarding indentation and other info.
keyseq([], []).
keyseq([key(_, S)|SS], [S|SubRet]) :- keyseq(SS, SubRet).


% Extract the sequence of values into a single (multi-line) string value.
vals_as_val([val(0, "")|VS], V) :- !, vals_as_val(VS, V).
vals_as_val(VS, V) :-
    reverse(VS, [val(0, "")|RVS]), !,
    reverse(RVS, VSS),
    vals_as_val(VSS, V).
vals_as_val(VS, V) :-
    maplist(raw_val, VS, RVS), intercalate(RVS, "\n", V).
raw_val(val(_, V), V).


%% ----------------------------------------------------------------------

%% Normalizes the parsed eqil.  If the input eqil is already normalized, this
%% will fail; if this succeeds, the second parameter is the normalized eqil.
%%
%% A normalized eqil may not be fully specified or fully consistent.  It is
%% strongly recommended that a normalized eqil be re-written (via emit_eqil)
%% and then re-parsed for a fully consistent specification.
%%
%% Normalization:
%%
%%  1. Replace blank keys with "parent.N" where parent is the parent key name,
%%     and N is a consecutive value.  NOTE: this key replacement ONLY occurs
%%     in the key portions, not in the value portion of the parents.
%%     To avoid an inconsistent eqil representation, this also removes the
%%     parent specification in which those keys would be part of the value.
%%     This removal might also affect the application depending on which
%%     key values it was consuming.  See the rewriting noted above.
%%
%%  2. Consolidate entries with the same parent.  For example:
%%        key1 =
%%          key2 = value2
%%        key1 =
%%          key3 = value3
%%     will be parsed as:
%%        [ eqil([key(0, "key1"), key(2, "key2")], [val(0, "value2")]),
%%          eqil([key(0, "key1")], [val(2, "key2 = value2")]),
%%          eqil([key(0, "key1"), key(2, "key3")], [val(0, "value3")]),
%%          eqil([key(0, "key1")], [val(2, "key3 = value3")])
%%        ]
%%     which can be normalized to:
%%        [ eqil([key(0, "key1")], [val(2, "key2 = value2"),
%%                                  val(2, "key3 = value3")]),
%%          eqil([key(0, "key1"), key(2, "key2")], [val(0, "value2")]),
%%          eqil([key(0, "key1"), key(2, "key3")], [val(0, "value3")])
%%        ]
%%     This consolidation may not affect an application consuming only the
%%     leaf values, but if the input can be consolidated it indicates that
%%     a rewrite of the EQIL file would contain changes, including re-ordering
%%     and consolidation at that level, which is often a beneficial "clean up"
%%     of the file.
%%
%%     Note that this will also combine values that have the same key:
%%        key = value1
%%        key = value2
%%        key = value3
%%     will become:
%%        key = value1
%%              value2
%%              value3

% KWQ TODO uniqueness against FULL EQIL from ALL files?
normalize_eqil([], _) :- !, fail.
normalize_eqil(InpEqil, OutEqil) :-
    ( assign_blank_keys(InpEqil, UpdEqil), !,
      consolidate_keys(UpdEqil, OutEqil, _)
    ; consolidate_keys(InpEqil, OutEqil, true)
    ).

assign_blank_keys(InpEqil, OutEqil) :-
    rewrite_blank_keys(InpEqil, InpEqil, 1, NewEqil, RemoveKeys),
    \+ RemoveKeys == [],
    remove_keys(NewEqil, RemoveKeys, OutEqil).
assign_blank_keys([], []).

rewrite_blank_keys(EQIL, [eqil([key(N,"")],V)|InpEqil], KeyVal,
                   [eqil([key(N,K)],V),UpdEqil], RemoveKeys) :-
    !,
    new_unique_key(EQIL, N, KeyVal, K, NextVal),
    rewrite_blank_keys(EQIL, InpEqil, NextVal, UpdEqil, RemoveKeys).
rewrite_blank_keys(EQIL, [eqil(K,V)|InpEqil], KeyVal,
                   [eqil(NewKey,V)|UpdEqil],
                   [K|RmvKeys]) :-
    reverse(K, [key(N, ""),key(NP,ParentKey)|KR]), !,
    new_unique_key(EQIL, KR, N, key(NP,ParentKey), ParentKey, KeyVal, NewKey, NextVal),
    rewrite_blank_keys(EQIL, InpEqil, NextVal, UpdEqil, RmvKeys).
rewrite_blank_keys(EQIL, [Eqil|InpEqil], KeyVal, [Eqil|OutEqil], RemoveKeys) :-
    rewrite_blank_keys(EQIL, InpEqil, KeyVal, OutEqil, RemoveKeys).
rewrite_blank_keys(_, [], _, [], []).

new_unique_key(EQIL, KR, N, PKey, Pfx, Val, OutKey, OutVal) :-
    succ(Val, NextVal),
    atom_string(Val, KVS),
    string_concat(Pfx, KVS, KS),
    reverse([key(N,KS),PKey|KR], NewKey),
    ( is_unique_key(EQIL, NewKey), !, OutVal = NextVal, OutKey = NewKey
    ; new_unique_key(EQIL, KR, N, PKey, Pfx, NextVal, OutKey, OutVal)
    ).

new_unique_key(EQIL, N, Val, OutKey, OutVal) :-
    succ(Val, NextVal),
    atom_string(Val, KS),
    reverse([key(N,KS)], NewKey),
    ( is_unique_key(EQIL, NewKey), !, OutVal = NextVal, OutKey = NewKey
    ; new_unique_key(EQIL, N, NextVal, OutKey, OutVal)
    ).

is_unique_key([], _).
is_unique_key([eqil(K,_)|_], K) :- !, fail.
is_unique_key([_|ES], K) :- is_unique_key(ES, K).

remove_keys([eqil(K,_)|InpEqil], RKeys, OutEqil) :-
    append(K, _, RKey),
    member(RKey, RKeys),
    !,
    remove_keys(InpEqil, RKeys, OutEqil).
remove_keys([Eqil|InpEqil], RKeys, [Eqil|OutEqil]) :-
    remove_keys(InpEqil, RKeys, OutEqil).
remove_keys([], _, []).

consolidate_keys([eqil(Key,Val)|InpEqil], [eqil(Key, JoinedVal)|REqil], Changed) :-
    join_keys(Key, Val, InpEqil, JoinedVal, RemainingEqil),
    consolidate_keys(RemainingEqil, REqil, SubChanged),
    (SubChanged, !, Changed = true
    ; Val == JoinedVal, !, Changed = false
    ; Changed = true
    ).
consolidate_keys([],[], false).

join_keys(_, Val, [], Val, []).
join_keys(Key, [val(0,V1)|Val], [eqil(Key, [val(0,V2)|Val2])|InpEqil],
          OtherVals, RemainingEqil) :-
    val_indent(Key, N),
    append([val(0,V1)|Val], [val(N, V2)|Val2], ValJ),
    join_keys(Key, ValJ, InpEqil, OtherVals, RemainingEqil).
join_keys(Key, [val(N,V1)|Val], [eqil(Key, [val(0,V2)|Val2])|InpEqil],
          OtherVals, RemainingEqil) :-
    append([val(0,V1)|Val], [val(N, V2)|Val2], ValJ),
    join_keys(Key, ValJ, InpEqil, OtherVals, RemainingEqil).
join_keys(Key, Val, [eqil(Key, Val2)|InpEqil], OtherVals, RemainingEqil) :-
    append(Val, Val2, ValJ),
    join_keys(Key, ValJ, InpEqil, OtherVals, RemainingEqil).
join_keys(Key, Val, [E|InpEqil], OtherVals, [E|RemainingEqil]) :-
    join_keys(Key, Val, InpEqil, OtherVals, RemainingEqil).

val_indent([], 8).  % unlikely situation
val_indent(Keys, I) :-
    reverse(Keys, [key(N, K)|_]),
    string_length(K, KL),
    sum_list([N, KL, 3], I).

%% ----------------------------------------------------------------------

emit_eqil(EQIL, String) :-
    gen_eqil_string(EQIL, [], _, StringRep),
    stringrep_to_string(StringRep, String).

stringrep_to_string(emptystr, "").
stringrep_to_string(emptyline, "").
stringrep_to_string(single(S), S).
stringrep_to_string(multi(S), S).
%% adj
%% adj_multi

% gen_eqil_string is top level. It should walk through the keys, extending the
% CurKeyPfx, to generate the results.
gen_eqil_string([eqil(K,V)|EQIL], CurKeyPfx, RemEqil, String) :-
    append(CurKeyPfx, [M|_], K), !, % K matches CurKeyPfx
    append(CurKeyPfx, [M], NxtKeyPfx),
    gen_eqil_match([eqil(K,V)|EQIL], NxtKeyPfx, MEqil, MString),
    gen_eqil_string(MEqil, CurKeyPfx, RemEqil, OString),
    gen_key([M], MS),
    gen_eqil_combine(MS, MString, OString, String).
gen_eqil_string([E|EQIL], CurKeyPfx, [E|RemEqil], String) :-
    gen_eqil_string(EQIL, CurKeyPfx, RemEqil, String).
gen_eqil_string([], _, [], emptystr).

% Determines how to emit this key and value, combined with the remainder of the
% eqil output:
%
%    gen_eqil_combine(Key, ValStringRep, FollowingStringRep, OutStringRep)
%
gen_eqil_combine(K, emptystr, emptystr, single(String)) :-
    format(atom(StringA), "~w =", [ K ]),
    atom_string(StringA, String).
gen_eqil_combine(K, emptystr, Sub, multi(String)) :-
    stringrep_to_string(Sub, SubStr),
    format(atom(StringA), "~w =~n~w", [ K, SubStr ]),
    atom_string(StringA, String).
gen_eqil_combine(K, emptyline, emptystr, single(String)) :-
    format(atom(StringA), "~w =~n", [ K ]),
    atom_string(StringA, String).
gen_eqil_combine(K, adj(V), emptystr, single(String)) :-
    format(atom(StringA), "~w = ~w", [ K, V ]),
    atom_string(StringA, String).
gen_eqil_combine(K, adj(V), Sub, single(String)) :-
    stringrep_to_string(Sub, SubStr),
    format(atom(StringA), "~w = ~w~n~w", [ K, V, SubStr ]),
    atom_string(StringA, String).
gen_eqil_combine(K, adj_multi(V,emptyline), emptystr, multi(String)) :-
    format(atom(StringA), "~w = ~w~n", [ K, V ]), %3
    atom_string(StringA, String).
gen_eqil_combine(K, adj_multi(V,emptyline), Sub, multi(String)) :-
    stringrep_to_string(Sub, SubStr),
    format(atom(StringA), "~w = ~w~n~n~w", [ K, V, SubStr ]), %3
    atom_string(StringA, String).
gen_eqil_combine(K, adj_multi("",ContV), emptystr, multi(String)) :-
    !, stringrep_to_string(ContV, VS),
    format(atom(StringA), "~w =~n~w", [ K, VS ]),
    atom_string(StringA, String).
gen_eqil_combine(K, adj_multi(V,ContV), emptystr, multi(String)) :-
    stringrep_to_string(ContV, VS),
    format(atom(StringA), "~w = ~w~n~w", [ K, V, VS ]),
    atom_string(StringA, String).
gen_eqil_combine(K, adj_multi(V,adj_multi(V1, V2)), Sub, multi(String)) :-
    !,
    gen_eqil_combine(K, adj_multi(V1, V2), Sub, SubRep),
    stringrep_to_string(SubRep, SubStr),
    format(atom(StringA), "~w = ~w~n~w", [ K, V, SubStr ]),
    atom_string(StringA, String).
gen_eqil_combine(K, adj_multi(V,adj(V1)), Sub, multi(String)) :-
    !,
    gen_eqil_combine(K, adj(V1), Sub, SubRep),
    stringrep_to_string(SubRep, SubStr),
    format(atom(StringA), "~w = ~w~n~w", [ K, V, SubStr ]),
    atom_string(StringA, String).
gen_eqil_combine(K, adj_multi(V,ContV), Sub, multi(String)) :-
    stringrep_to_string(ContV, VS),
    stringrep_to_string(Sub, SubStr),
    format(atom(StringA), "~w = ~w~n~w~n~w", [ K, V, VS, SubStr ]),
    atom_string(StringA, String).
gen_eqil_combine(K, single(VS), emptystr, single(String)) :-
    format(atom(StringA), "~w =~n~w", [ K, VS ]),
    atom_string(StringA, String).
gen_eqil_combine(K, single(VS), single(OString), multi(String)) :-
    format(atom(StringA), "~w =~n~w~n~w", [ K, VS, OString ]),
    atom_string(StringA, String).
gen_eqil_combine(K, single(VS), multi(OString), multi(String)) :-
    format(atom(StringA), "~w =~n~w~n~w", [ K, VS, OString ]),
    atom_string(StringA, String).
gen_eqil_combine(K, multi(VS), emptystr, multi(String)) :-
    format(atom(StringA), "~w =~n~w", [ K, VS ]),
    atom_string(StringA, String).
gen_eqil_combine(K, multi(VS), Sub, multi(String)) :-
    stringrep_to_string(Sub, SubStr),
    format(atom(StringA), "~w =~n~w~n~w", [ K, VS, SubStr ]),
    atom_string(StringA, String).
gen_eqil_combine(K, emptyline, multi(Sub), multi(String)) :-
    format(atom(StringA), "~w =~n~n~w", [ K, Sub ]),
    atom_string(StringA, String).
gen_eqil_combine(K, emptyline, single(Sub), multi(String)) :-
    format(atom(StringA), "~w =~n~n~w", [ K, Sub ]),
    atom_string(StringA, String).

% gen_eqil_match is called to get all sub-elements that match the locked-in
% CurKeyPfx and return those in StringRep and the non-matches in RemEqil.
gen_eqil_match([eqil(CurKeyPfx,V)|EQIL], CurKeyPfx, RemEqil, StringRep) :-
    !, gen_eqil_string(EQIL, CurKeyPfx, RemEqil, RStr),
    gen_eqil_check(V, RStr, StringRep).
gen_eqil_match([eqil(K,V)|EQIL], CurKeyPfx, RemEqil, StringRep) :-
    append(CurKeyPfx, _, K), !,
    % This is not a terminal key, so filter out any EQIL elements that terminate
    % on this key
    remove_keymatch(CurKeyPfx, EQIL, FilteredEQIL),
    % expecting CurKeyPfx to advance!
    gen_eqil_string([eqil(K,V)|FilteredEQIL], CurKeyPfx, RemEqil, StringRep).

gen_eqil_check(V, emptystr, StringRep) :-
    % current eqil was the only thing that matched CurKeyPfx
    !, gen_val(V, StringRep).
gen_eqil_check(_, RStr, RStr).

remove_keymatch(_, [], []).
remove_keymatch(K, [eqil(K,_)|ES], EQIL) :- !, remove_keymatch(K, ES, EQIL).
remove_keymatch(K, [E|ES], [E|EQIL]) :- remove_keymatch(K, ES, EQIL).

gen_key([], "").
gen_key([key(N,K)|KS], String) :-
    gen_key(KS, ""), !,
    gen_spaces(N, S),
    format(atom(KA), "~w~w", [S,K]),
    atom_string(KA, String).
gen_key([key(N,K)|KS], String) :-
    gen_key(KS, SS),
    gen_spaces(N, S),
    format(atom(KA), "~w~w~n~w", [S,K,SS]),
    atom_string(KA, String).

gen_val([], emptystr) :- !.
gen_val([val(0,"")], emptyline) :- !.
gen_val([val(0,V)], adj(V)) :- !.
gen_val([val(0,V)|VS], adj_multi(V,SubString)) :- !, gen_val_(VS, SubString).
gen_val(V, StringRep) :-
    gen_val_(V, ValRep),
    ( as_valblock(ValRep), !, StringRep = adj_multi("|", ValRep)
    ; StringRep = ValRep
    ).

gen_val_([], emptystr) :- !.
gen_val_([val(N,V)|VS], StringRep) :-
    gen_spaces(N, S), !,
    gen_val_(VS, SubString),
    ret_val(SubString, S, V, StringRep).

gen_spaces(0, "").
gen_spaces(N, S) :- succ(P, N), gen_spaces(P, Q), string_concat(" ", Q, S).

ret_val(emptystr, S, V, single(R)) :- format(atom(R), "~w~w", [S, V]).
ret_val(emptyline, S, V, multi(R)) :- format(atom(R), "~w~w~n", [S, V]).
ret_val(single(SubString), S, V, multi(R)) :-
    format(atom(R), "~w~w~n~w", [ S, V, SubString ]).
ret_val(multi(SubString), S, V, multi(R)) :-
    format(atom(R), "~w~w~n~w", [ S, V, SubString ]).
ret_val(adj_multi(SubString,multi(MoreSub)), S, V, multi(R)) :-
    format(atom(R), "~w~w~n~w~n~w", [ S, V, SubString, MoreSub ]).
%% ret_val(X, S, V, _) :- writeln(ret_val_fail), writeln(X), fail.

as_valblock(single(String)) :- !, string_chars(String, Codes), member('=', Codes).
as_valblock(multi(String)) :- !, string_chars(String, Codes), member('=', Codes).
as_valblock(String) :- string_chars(String, Codes), member('=', Codes).
