:- begin_tests(eqil).
:- use_module(eqil).
:- use_module('../englib').
:- use_module(library(strings)).

% Note: many tests are nondet because the show_warnings has separate predicates
% to handle no-warnings v.s. some-warnings, which leaves a choicepoint even
% though only one will be used.

test(empty, [nondet]) :-
    Inp = {|string||
|
|},
    check(Inp, [], _).
    %% writeln(Result),
    %% assertion(Result == []).

test(simple_key_val, [nondet]) :-
    Inp = {|string||
| key = value
|},
    check(Inp,
          [ eqil([key(0, "key")], [val(0, "value")])
          ], Result),
    % n.b. Do not want asserted eng:key and eng:eng predicates from this test to
    % affect other tests.  We cannot perform the assert_eqil and the subsequent
    % tests inside of a snapshot because assertion failures in a snapshot are not
    % propagated out (so all tests automatically succeed).  Instead, use
    % revert_assert_eng to remove all eng:key and eng:eng predicates asserted.
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    revert_assert_eng.


test(multiple_key_val, [nondet]) :-
    Inp = {|string||
|
| key = value
| key2 = value2
|
| another key = another value
|},
    check(Inp,
          [ eqil([key(0, "key")], [val(0, "value")]),
            eqil([key(0, "key2")], [val(0, "value2"), val(0, "")]),
            eqil([key(0, "another key")], [val(0, "another value")])
          ],
          Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "value2")),
    assertion(eng:key('another key')),
    assertion(eng:eng('another key', "another value")),
    revert_assert_eng.


test(key_only, [nondet]) :-
    Inp = {|string||
| key = value
| key2 = value2
|
| another key = another value
| just a key =
|},
    check(Inp,
          [ eqil([key(0, "key")], [val(0, "value")]),
            eqil([key(0, "key2")], [val(0, "value2"), val(0, "")]),
            eqil([key(0, "another key")], [val(0, "another value")]),
            eqil([key(0, "just a key")], [])
          ],
          Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "value2")),
    assertion(eng:key('another key')),
    assertion(eng:eng('another key', "another value")),
    assertion(eng:key('just a key')),
    revert_assert_eng.


test(multi_line_value, [nondet]) :-
    Inp = {|string||
| key = value
| key2 = This is a multi-line value
|        that is on two lines.
| another key = another value
|
|},
    check(Inp,
          [ eqil([key(0, "key")], [val(0, "value")]),
            eqil([key(0, "key2")], [val(0, "This is a multi-line value"),
                                    val(7, "that is on two lines.")
                                   ]),
            eqil([key(0, "another key")], [val(0, "another value")
                                          ])
          ],
          Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is a multi-line value\nthat is on two lines.")),
    assertion(eng:key('another key')),
    assertion(eng:eng('another key', "another value")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, key2, 'another key' ]),
    %% findall((K,V), eng:eng(K,V), KVS),
    %% writeln(KVS),
    revert_assert_eng.


test(multi_line_value_implicit_key, [nondet]) :-
    Inp = {|string||
|
| key = value
| key2 = This is a multi-line value
|        that is on two lines.
| another key
|   another value
|
|},
    % When emitting, keys always have a following =
    Out = {|string||
|
| key = value
| key2 = This is a multi-line value
|        that is on two lines.
| another key =
|   another value
|
|},
    Parsed = [ eqil([key(0, "key")], [val(0, "value")]),
               eqil([key(0, "key2")], [val(0, "This is a multi-line value"),
                                       val(7, "that is on two lines.")]),
               eqil([key(0, "another key")], [val(2, "another value")
                                             ])
             ],
    check(Inp, Parsed, Out, Parsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is a multi-line value\nthat is on two lines.")),
    assertion(eng:key('another key')),
    assertion(eng:eng('another key', "another value")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, key2, 'another key' ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(multi_line_value_with_empty_trailing, [nondet]) :-
    Inp = {|string||
| key = value
| key2 = This is a multi-line value
|        that is on two lines.
|
| another key = another value
|},
    check(Inp,
          [ eqil([key(0, "key")], [val(0, "value")]),
            eqil([key(0, "key2")], [val(0, "This is a multi-line value"),
                                    val(7, "that is on two lines."),
                                    val(0, "")]),
            eqil([key(0, "another key")], [val(0, "another value")])
          ],
          Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is a multi-line value\nthat is on two lines.")),
    assertion(eng:key('another key')),
    assertion(eng:eng('another key', "another value")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, key2, 'another key' ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(multi_line_indented_value, [nondet]) :-
    Inp = {|string||
|
| key = value
| key2 =
|    This is a multi-line value
|        that is on three lines
|   and this is the third.
| another key = another value
|
|},
    % When emitting, keys always have a following =
    Out = {|string||
|
| key = value
| key2 =
|    This is a multi-line value =
|        that is on three lines
|   and this is the third. =
| another key = another value
|
|},
    E1 = eqil([key(0, "key")], [val(0, "value")]),
    E2 = eqil([key(0, "key2"), key(3, "This is a multi-line value")],
              [val(7, "that is on three lines")]),
    E3 = eqil([key(0, "key2"), key(2, "and this is the third.")], []),
    E4 = eqil([key(0, "key2")], [val(3, "This is a multi-line value"),
                                 val(7, "that is on three lines"),
                                 val(2, "and this is the third.")]),
    E5 = eqil([key(0, "another key")], [val(0, "another value")]),
    Parsed = [ E1, E2, E3, E4, E5 ],
    Normalized = [
        E1, E2, E3,
        eqil([key(0, "key2")], [val(3, "This is a multi-line value ="),
                                val(7, "that is on three lines"),
                                val(2, "and this is the third. =")
                                %% val(0, "")
                               ]),
        E5
    ],
    check(Inp, Parsed, Out, Normalized, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is a multi-line value =\nthat is on three lines\nand this is the third. =")),
    assertion(eng:key('another key')),
    assertion(eng:eng('another key', "another value")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, key2, 'another key' ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.

test(multi_line_indented_value_with_blanks, [nondet]) :-
    Inp = {|string||
|
|     key = value
|
| key2 =
|
|    This is a multi-line value
|        that is on five lines
|      staggered leftwards
|   and this is the fourth
|             except the last.
|
| another key = another value
|
|},
    %% keys are always followed by =
    Out = {|string||
|
|     key = value
|
| key2 =
|    This is a multi-line value =
|        that is on five lines
|      staggered leftwards
|   and this is the fourth =
|             except the last.
|
| another key = another value
|
|},
    Parsed = [
        eqil([key(4, "key")], [val(0, "value"), val(0, "")]),
        eqil([key(0, "key2"), key(3, "This is a multi-line value")],
             [val(7, "that is on five lines"),
              val(5, "staggered leftwards")
             ]),
        eqil([key(0, "key2"), key(2, "and this is the fourth")],
             [val(12, "except the last."), val(0, "")]),
        eqil([key(0, "key2")], [val(0, ""),
                                val(3, "This is a multi-line value"),
                                val(7, "that is on five lines"),
                                val(5, "staggered leftwards"),
                                val(2, "and this is the fourth"),
                                val(12, "except the last."),
                                val(0, "")]),
        eqil([key(0, "another key")], [val(0, "another value")])
    ],
    %% Same as Parsed except key2 where sub-keys are followed by =
    Normalized = [
        eqil([key(4, "key")], [val(0, "value"), val(0, "")]),
        eqil([key(0, "key2"), key(3, "This is a multi-line value")],
             [val(7, "that is on five lines"),
              val(5, "staggered leftwards")
             ]),
        eqil([key(0, "key2"), key(2, "and this is the fourth")],
             [val(12, "except the last."), val(0, "")]),
        eqil([key(0, "key2")], [val(3, "This is a multi-line value ="),
                                val(7, "that is on five lines"),
                                val(5, "staggered leftwards"),
                                val(2, "and this is the fourth ="),
                                val(12, "except the last."),
                                val(0, "")]),
        eqil([key(0, "another key")], [val(0, "another value")])
    ],
    check(Inp, Parsed, Out, Normalized, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is a multi-line value =\nthat is on five lines\nstaggered leftwards\nand this is the fourth =\nexcept the last.")),
    assertion(eng:key('another key')),
    assertion(eng:eng('another key', "another value")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, key2, 'another key' ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.

test(multi_line_value_only, [nondet]) :-
    Inp = {|string||
| key = value
|
| key2 =
|    This is = a multi-line value
|        that is on three lines =
|   = and = this is the third.
| another key = another value
|
|},
    % This one is unusual: not only is there a blank key that normalize will fill
    % in, but there is a key with a value that also has sub-keys.  At the present
    % time, eqil does not support having a key with a value *and* sub-keys, and
    % the value itself will be dropped on emitting/
    Out = {|string||
| key = value
|
| key2 =
|    This is =
|        that is on three lines =
|   key21 = and = this is the third.
| another key = another value
|
|},
    Parsed = [
        eqil([key(0, "key")], [val(0, "value"), val(0, "")]),
        eqil([key(0, "key2"),
              key(3, "This is"),
              key(7, "that is on three lines")],
             []),
        eqil([key(0, "key2"), key(3, "This is")],
             [val(0, "a multi-line value"),
              val(7, "that is on three lines =")]),
        eqil([key(0, "key2"), key(2, "")],
             [val(0, "and = this is the third.")]),
        eqil([key(0, "key2")], [val(3, "This is = a multi-line value"),
                                val(7, "that is on three lines ="),
                                val(2, "= and = this is the third.")]),
        eqil([key(0, "another key")], [val(0, "another value")])
        ],
    Normalized = [
        eqil([key(0, "key")], [val(0, "value"), val(0, "")]),
        eqil([key(0, "key2"),
              key(3, "This is"),
              key(7, "that is on three lines")],
             []),
        eqil([key(0, "key2"), key(3, "This is")],
             [val(0, "a multi-line value"),
              val(7, "that is on three lines =")]),
        % Removed because key21 supplied below
        %% eqil([key(0, "key2")], [val(3, "This is = a multi-line value"),
        %%                         val(7, "that is on three lines ="),
        %%                         val(2, "= and = this is the third.")]),
        eqil([key(0, "key2"),
              key(2, "key21")],
             [val(0, "and = this is the third.")]),
        eqil([key(0, "another key")], [val(0, "another value")])
    ],
    ReParsed = [
        eqil([key(0, "key")], [val(0, "value"), val(0, "")]),
        % emitting removes the value if there are sub-keys
        eqil([key(0, "key2"),
              key(3, "This is"),
              key(7, "that is on three lines")],
             []),
        eqil([key(0, "key2"), key(3, "This is")],
             [% val(0, "a multi-line value"),
              val(7, "that is on three lines =")]),
        eqil([key(0, "key2"),
              key(2, "key21")],
             [val(0, "and = this is the third.")]),
        % Re-parsing sees the new version of this
        eqil([key(0, "key2")], [val(3, "This is ="),
                                val(7, "that is on three lines ="),
                                val(2, "key21 = and = this is the third.")]),
        eqil([key(0, "another key")], [val(0, "another value")])
    ],
    check(Inp, Parsed, Out, Normalized, ReParsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is =\nthat is on three lines =\nkey21 = and = this is the third.")),
    assertion(eng:key(key2, 'This is')),
    assertion(eng:eng(key2, 'This is', "that is on three lines =")),
    assertion(eng:key(key2, 'key21')),
    assertion(eng:eng(key2, 'key21', "and = this is the third.")),
    assertion(eng:key('another key')),
    assertion(eng:eng('another key', "another value")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, key2, 'another key' ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.

test(multi_line_value_only_with_blanks, [nondet]) :-
    Inp = {|string||
| key = value
|
| key2 =
|
|    This is = a multi-line value
|        that is on three lines =
|   = and = this is the third.
|     and the fourth
|
| another key = another value
|
|},
    % Blank lines between the key and value are dropped, if a key has a value and
    % subkeys, the value is dropped, and blank keys are auto-generated.
    Out = {|string||
| key = value
|
| key2 =
|    This is =
|        that is on three lines =
|   key21 = and = this is the third.
|     and the fourth
|
| another key = another value
|
|},
    Parsed = [
        eqil([key(0, "key")], [val(0, "value"), val(0, "")]),
        eqil([key(0, "key2"),
              key(3, "This is"),
              key(7, "that is on three lines")],
             []),
        eqil([key(0, "key2"), key(3, "This is")],
             [val(0, "a multi-line value"),
              val(7, "that is on three lines =")]),
        eqil([key(0, "key2"), key(2, "")],
             [val(0, "and = this is the third."),
              val(4, "and the fourth"),
              val(0, "")]),
        eqil([key(0, "key2")], [val(0, ""),
                                val(3, "This is = a multi-line value"),
                                val(7, "that is on three lines ="),
                                val(2, "= and = this is the third."),
                                val(4, "and the fourth"),
                                val(0, "")]),
        eqil([key(0, "another key")], [val(0, "another value")])
    ],
    % Generates a value for the previously blank key and removes parent key
    % values
    Normalized = [
        eqil([key(0, "key")], [val(0, "value"), val(0, "")]),
        eqil([key(0, "key2"),
              key(3, "This is"),
              key(7, "that is on three lines")],
             []),
        eqil([key(0, "key2"), key(3, "This is")],
             [val(0, "a multi-line value"),
              val(7, "that is on three lines =")]),
        %% eqil([key(0, "key2")], [val(0, ""),
        %%                         val(3, "This is = a multi-line value"),
        %%                         val(7, "that is on three lines ="),
        %%                         val(2, "= and = this is the third."),
        %%                         val(4, "and the fourth"),
        %%                         val(0, "")]),
        eqil([key(0, "key2"),
              key(2, "key21")],
             [val(0, "and = this is the third."),
              val(4, "and the fourth"),
              val(0, "")]),
        eqil([key(0, "another key")], [val(0, "another value")])
    ],
    % Adds the generated key value replacing the blank key and re-introduces the
    % parent key value, but drops the value for keys with value and sub-keys.
    ReParsed = [
        eqil([key(0, "key")], [val(0, "value"), val(0, "")]),
        eqil([key(0, "key2"),
              key(3, "This is"),
              key(7, "that is on three lines")],
             []),
        eqil([key(0, "key2"), key(3, "This is")],
             [ % val(0, "a multi-line value"),
              val(7, "that is on three lines =")]),
        eqil([key(0, "key2"),
              key(2, "key21")],
             [val(0, "and = this is the third."),
              val(4, "and the fourth"),
              val(0, "")]),
        eqil([key(0, "key2")], [val(3, "This is ="),
                                val(7, "that is on three lines ="),
                                val(2, "key21 = and = this is the third."),
                                val(4, "and the fourth"),
                                val(0, "")]),
        eqil([key(0, "another key")], [val(0, "another value")])
    ],
    check(Inp, Parsed, Out, Normalized, ReParsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is =\nthat is on three lines =\nkey21 = and = this is the third.\nand the fourth")),
    assertion(eng:key(key2, 'This is')),
    assertion(eng:eng(key2, 'This is', "that is on three lines =")),
    assertion(eng:key(key2, 'This is', 'that is on three lines')),
    assertion(eng:key(key2, 'key21')),
    assertion(eng:eng(key2, 'key21', "and = this is the third.\nand the fourth")),
    assertion(eng:key('another key')),
    assertion(eng:eng('another key', "another value")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, key2, 'another key' ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(nested_keyvals, [nondet]) :-
    Inp = {|string||
| key =
|   subkey = subval1
|   subkey2 = subval2
| another key =
|   ! = another value with a blank key
|},
    Parsed = [
        eqil([key(0, "key"), key(2, "subkey")], [val(0, "subval1")]),
        eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
        eqil([key(0, "key")],
             [val(2, "subkey = subval1"),
              val(2, "subkey2 = subval2")
             ]),
        eqil([key(0, "another key"), key(2, "!")],
             [val(0, "another value with a blank key")]),
        eqil([key(0, "another key")],
             [val(2, "! = another value with a blank key")])
    ],
    check(Inp, Parsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:key(key, subkey)),
    assertion(eng:eng(key, subkey, "subval1")),
    assertion(eng:key(key, subkey2)),
    assertion(eng:eng(key, subkey2, "subval2")),
    assertion(eng:key('another key')),
    assertion(eng:key('another key', '!')),
    assertion(eng:eng('another key', '!', "another value with a blank key")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, 'another key' ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(nested_keyvals_with_implicit_key, [nondet]) :-
    Inp = {|string||
|
| key =
|   subkey = subval1
|   subkey2 = subval2
| another key
|   ! = another value with a blank key
|},
    % all keys have an =
    Out = {|string||
|
| key =
|   subkey = subval1
|   subkey2 = subval2
| another key =
|   ! = another value with a blank key
|},
    Parsed = [
        eqil([key(0, "key"), key(2, "subkey")], [val(0, "subval1")]),
        eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
        eqil([key(0, "key")],
             [val(2, "subkey = subval1"),
              val(2, "subkey2 = subval2")
             ]),
        eqil([key(0, "another key"), key(2, "!")],
             [val(0, "another value with a blank key")]),
        eqil([key(0, "another key")],
             [val(2, "! = another value with a blank key")])
    ],
    check(Inp, Parsed, Out, Parsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:key(key, subkey)),
    assertion(eng:eng(key, subkey, "subval1")),
    assertion(eng:key(key, subkey2)),
    assertion(eng:eng(key, subkey2, "subval2")),
    assertion(eng:key('another key')),
    assertion(eng:key('another key', '!')),
    assertion(eng:eng('another key', '!', "another value with a blank key")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, 'another key' ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(nested_keyvals_with_blank_keys, [nondet]) :-
    Inp = {|string||
| key =
|   subkey = subval1
|   subkey2 = subval2
| another key
|   = another value with a blank key
|},
    % all keys have an = and blank keys are replaced
    Out = {|string||
| key =
|   subkey = subval1
|   subkey2 = subval2
| another key =
|   another key1 = another value with a blank key
|},
    Parsed = [
        eqil([key(0, "key"), key(2, "subkey")], [val(0, "subval1")]),
        eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
        eqil([key(0, "key")],
             [val(2, "subkey = subval1"),
              val(2, "subkey2 = subval2")
             ]),
        eqil([key(0, "another key"), key(2, "")],
             [val(0, "another value with a blank key")]),
        eqil([key(0, "another key")],
             [val(2, "= another value with a blank key")])
    ],
    Normalized = [
        eqil([key(0, "key"), key(2, "subkey")], [val(0, "subval1")]),
        eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
        eqil([key(0, "key")],
             [val(2, "subkey = subval1"),
              val(2, "subkey2 = subval2")
             ]),
        eqil([key(0, "another key"),
              key(2, "another key1")],
             [val(0, "another value with a blank key")])
        %% eqil([key(0, "another key")],
        %%      [val(2, "= another value with a blank key")])
    ],
    ReParsed = [
        eqil([key(0, "key"), key(2, "subkey")], [val(0, "subval1")]),
        eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
        eqil([key(0, "key")],
             [val(2, "subkey = subval1"),
              val(2, "subkey2 = subval2")
             ]),
        eqil([key(0, "another key"),
              key(2, "another key1")],
             [val(0, "another value with a blank key")]),
        eqil([key(0, "another key")],
             [val(2, "another key1 = another value with a blank key")])
    ],
    check(Inp, Parsed, Out, Normalized, ReParsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:key(key, subkey)),
    assertion(eng:eng(key, subkey, "subval1")),
    assertion(eng:key(key, subkey2)),
    assertion(eng:eng(key, subkey2, "subval2")),
    assertion(eng:key('another key')),
    assertion(eng:key('another key', 'another key1')),
    assertion(eng:eng('another key', 'another key1', "another value with a blank key")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, 'another key' ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(nested_nested_keyvals, [nondet]) :-
    Inp = {|string||
| key =
|   subkey =
|     <first_subkey> = #1!
|     another key = double sub
|                   multi-line value
|     key with no value
|
|     and
|
|       an implicit key above with a multi-line value here
|       and here
|
|       that contains blanks
|       internally, preceeding, and following.
|
|   subkey2 = subval2
|   subkey3 =
|     sub key 3.1 =
|       one = 1
|       2 = two
|},
    % Removes blank lines between key and value, all keys have an =
    Out = {|string||
| key =
|   subkey =
|     <first_subkey> = #1!
|     another key = double sub
|                   multi-line value
|     key with no value =
|
|     and =
|       an implicit key above with a multi-line value here
|       and here
|
|       that contains blanks
|       internally, preceeding, and following.
|
|   subkey2 = subval2
|   subkey3 =
|     sub key 3.1 =
|       one = 1
|       2 = two
|},
    Exp = [ eqil([key(0, "key"), key(2, "subkey"), key(4, "<first_subkey>")],
                 [val(0, "#1!")]),
            eqil([key(0, "key"), key(2, "subkey"), key(4, "another key")],
                 [val(0, "double sub"), val(18, "multi-line value")]),
            eqil([key(0, "key"), key(2, "subkey"), key(4, "key with no value")],
                 [val(0, "")]),
            eqil([key(0, "key"), key(2, "subkey"), key(4, "and")],
                 [val(0, ""),
                  val(6, "an implicit key above with a multi-line value here"),
                  val(6, "and here"),
                  val(0, ""),
                  val(6, "that contains blanks"),
                  val(6, "internally, preceeding, and following."),
                  val(0, "")
                 ]),
            eqil([key(0, "key"), key(2, "subkey")],
                 [val(4, "<first_subkey> = #1!"),
                  val(4, "another key = double sub"),
                  val(18, "multi-line value"),
                  val(4, "key with no value"),
                  val(0, ""),
                  val(4, "and"),
                  val(0, ""),
                  val(6, "an implicit key above with a multi-line value here"),
                  val(6, "and here"),
                  val(0, ""),
                  val(6, "that contains blanks"),
                  val(6, "internally, preceeding, and following."),
                  val(0, "")
                 ]),
            eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
            eqil([key(0, "key"), key(2, "subkey3"), key(4, "sub key 3.1"),
                  key(6, "one")],
                 [val(0, "1")]),
            eqil([key(0, "key"), key(2, "subkey3"), key(4, "sub key 3.1"),
                  key(6, "2")],
                 [val(0, "two")]),
            eqil([key(0, "key"), key(2, "subkey3"), key(4, "sub key 3.1")],
                 [val(6, "one = 1"),
                  val(6, "2 = two")
                 ]),
            eqil([key(0, "key"), key(2, "subkey3")],
                 [val(4, "sub key 3.1 ="),
                  val(6, "one = 1"),
                  val(6, "2 = two")
                 ]),
            eqil([key(0, "key")],
                 [val(2, "subkey ="),
                  val(4, "<first_subkey> = #1!"),
                  val(4, "another key = double sub"),
                  val(18, "multi-line value"),
                  val(4, "key with no value"),
                  val(0, ""),
                  val(4, "and"),
                  val(0, ""),
                  val(6, "an implicit key above with a multi-line value here"),
                  val(6, "and here"),
                  val(0, ""),
                  val(6, "that contains blanks"),
                  val(6, "internally, preceeding, and following."),
                  val(0, ""),
                  val(2, "subkey2 = subval2"),
                  val(2, "subkey3 ="),
                  val(4, "sub key 3.1 ="),
                  val(6, "one = 1"),
                  val(6, "2 = two")
                 ])
          ],
    % keys all have = and no blank lines between keys and their values
    ReParsed = [
        eqil([key(0, "key"), key(2, "subkey"), key(4, "<first_subkey>")],
             [val(0, "#1!")]),
        eqil([key(0, "key"), key(2, "subkey"), key(4, "another key")],
             [val(0, "double sub"), val(18, "multi-line value")]),
        eqil([key(0, "key"), key(2, "subkey"), key(4, "key with no value")],
             [val(0, "")]),
        eqil([key(0, "key"), key(2, "subkey"), key(4, "and")],
             [% val(0, ""),
              val(6, "an implicit key above with a multi-line value here"),
              val(6, "and here"),
              val(0, ""),
              val(6, "that contains blanks"),
              val(6, "internally, preceeding, and following."),
              val(0, "")
             ]),
        eqil([key(0, "key"), key(2, "subkey")],
             [val(4, "<first_subkey> = #1!"),
              val(4, "another key = double sub"),
              val(18, "multi-line value"),
              val(4, "key with no value ="),
              val(0, ""),
              val(4, "and ="),
              %% val(0, ""),
              val(6, "an implicit key above with a multi-line value here"),
              val(6, "and here"),
              val(0, ""),
              val(6, "that contains blanks"),
              val(6, "internally, preceeding, and following."),
              val(0, "")
             ]),
        eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
        eqil([key(0, "key"), key(2, "subkey3"), key(4, "sub key 3.1"),
              key(6, "one")],
             [val(0, "1")]),
        eqil([key(0, "key"), key(2, "subkey3"), key(4, "sub key 3.1"),
              key(6, "2")],
             [val(0, "two")]),
        eqil([key(0, "key"), key(2, "subkey3"), key(4, "sub key 3.1")],
             [val(6, "one = 1"),
              val(6, "2 = two")
             ]),
        eqil([key(0, "key"), key(2, "subkey3")],
             [val(4, "sub key 3.1 ="),
              val(6, "one = 1"),
              val(6, "2 = two")
             ]),
        eqil([key(0, "key")],
             [val(2, "subkey ="),
              val(4, "<first_subkey> = #1!"),
              val(4, "another key = double sub"),
              val(18, "multi-line value"),
              val(4, "key with no value ="),
              val(0, ""),
              val(4, "and ="),
              %% val(0, ""),
              val(6, "an implicit key above with a multi-line value here"),
              val(6, "and here"),
              val(0, ""),
              val(6, "that contains blanks"),
              val(6, "internally, preceeding, and following."),
              val(0, ""),
              val(2, "subkey2 = subval2"),
              val(2, "subkey3 ="),
              val(4, "sub key 3.1 ="),
              val(6, "one = 1"),
              val(6, "2 = two")
             ])
    ],
    check(Inp, Exp, Out, already_normalized, ReParsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:key(key, subkey)),
    assertion(eng:key(key, subkey, '<first_subkey>')),
    assertion(eng:eng(key, subkey, '<first_subkey>', "#1!")),
    assertion(eng:key(key, subkey, 'another key')),
    assertion(eng:eng(key, subkey, 'another key', "double sub\nmulti-line value")),
    assertion(eng:key(key, subkey, 'key with no value')),
    assertion(eng:key(key, subkey, 'and')),
    assertion(eng:eng(key, subkey, 'and', "an implicit key above with a multi-line value here\nand here\n\nthat contains blanks\ninternally, preceeding, and following.")),
    assertion(eng:key(key, subkey2)),
    assertion(eng:eng(key, subkey2, "subval2")),
    assertion(eng:key(key, subkey3)),
    % TODO: align values, so that "one" and "2" are both preceeded by two spaces
    % in the following test?
    assertion(eng:eng(key, subkey3, "sub key 3.1 =\none = 1\n2 = two")),
    assertion(eng:key(key, subkey3, 'sub key 3.1')),
    assertion(eng:eng(key, subkey3, 'sub key 3.1', "one = 1\n2 = two")),
    assertion(eng:key(key, subkey3, 'sub key 3.1', 'one')),
    assertion(eng:eng(key, subkey3, 'sub key 3.1', 'one', "1")),
    assertion(eng:key(key, subkey3, 'sub key 3.1', '2')),
    assertion(eng:eng(key, subkey3, 'sub key 3.1', '2', "two")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key ]),
    findall((K1,K2), eng:key(K1,K2), K2S),
    assertion(K2S == [ (key,subkey), (key,subkey2), (key,subkey3) ]),
    findall((K1,K2,K3), eng:key(K1,K2,K3), K3S),
    assertion(K3S == [ (key,subkey,'<first_subkey>'),
                       (key,subkey,'another key'),
                       (key,subkey,'key with no value'),
                       (key,subkey,'and'),
                       (key,subkey3,'sub key 3.1')
                     ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(value_reconstruction_is_valid, [nondet]) :-
    Inp = {|string||
| key =
|   exec =
|     command --debug=true --inline=yes inpfile
| another key
|   = another value with a blank key
|},
    % All keys have = with surrounding spaces, assigned keys for blanks
    % KWQ: need to suppress the addition of spaces around =
    Out = {|string||
| key =
|   exec =
|     command --debug = true --inline=yes inpfile
| another key =
|   another key1 = another value with a blank key
|},
    Parsed = [
        eqil([key(0, "key"), key(2, "exec"), key(4, "command --debug")],
             [val(0, "true --inline=yes inpfile")]),
        eqil([key(0, "key"), key(2, "exec")],
             [val(4, "command --debug=true --inline=yes inpfile")]),
        eqil([key(0, "key")],
             [val(2, "exec ="),
              val(4, "command --debug=true --inline=yes inpfile")
             ]),
        eqil([key(0, "another key"), key(2, "")],
             [val(0, "another value with a blank key")]),
        eqil([key(0, "another key")],
             [val(2, "= another value with a blank key")])
    ],
    Normalized = [
        eqil([key(0, "key"), key(2, "exec"), key(4, "command --debug")],
             [val(0, "true --inline=yes inpfile")]),
        eqil([key(0, "key"), key(2, "exec")],
             [val(4, "command --debug=true --inline=yes inpfile")]),
        eqil([key(0, "key")],
             [val(2, "exec ="),
              val(4, "command --debug=true --inline=yes inpfile")
             ]),
        eqil([key(0, "another key"), key(2, "another key1")],
             [val(0, "another value with a blank key")])
        %% eqil([key(0, "another key")],
        %%      [val(2, "= another value with a blank key")])
    ],
    ReParsed = [
        eqil([key(0, "key"), key(2, "exec"), key(4, "command --debug")],
             [val(0, "true --inline=yes inpfile")]),
        eqil([key(0, "key"), key(2, "exec")],
             [val(4, "command --debug = true --inline=yes inpfile")]),
        eqil([key(0, "key")],
             [val(2, "exec ="),
              val(4, "command --debug = true --inline=yes inpfile")
             ]),
        eqil([key(0, "another key"), key(2, "another key1")],
             [val(0, "another value with a blank key")]),
        eqil([key(0, "another key")],
             [val(2, "another key1 = another value with a blank key")])
    ],
    check(Inp, Parsed, Out, Normalized, ReParsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:key(key, exec)),
    assertion(eng:eng(key, exec, "command --debug = true --inline=yes inpfile")),
    assertion(eng:key(key, exec, 'command --debug')),
    assertion(eng:eng(key, exec, 'command --debug', "true --inline=yes inpfile")),
    assertion(eng:key('another key')),
    assertion(eng:key('another key', 'another key1')),
    assertion(eng:eng('another key', 'another key1', "another value with a blank key")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, 'another key' ]),
    findall((K1,K2), eng:key(K1,K2), K2S),
    assertion(K2S == [ (key,exec), ('another key','another key1') ]),
    findall((K1,K2,K3), eng:key(K1,K2,K3), K3S),
    assertion(K3S == [ (key,exec,'command --debug') ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(valueless_cascade, [nondet]) :-
    Inp = {|string||
| key =
|   op =
|     build =
|       help = build operation for op
|       exec =
|         command --debug=true --inline=yes inpfile
|       #= This is=my comment style =#
|
|     run =
|       help = run me
|       exec = run it
|},
    % spaces around =, adds an extra blank line for %3 gen_eqil_combine
    Out = {|string||
| key =
|   op =
|     build =
|       help = build operation for op
|       exec =
|         command --debug = true --inline=yes inpfile
|       # = This is=my comment style =#
|
|     run =
|       help = run me
|       exec = run it
|},
    Parsed = [
        eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "help")],
             [val(0, "build operation for op")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "build"),
              key(6, "exec"), key(8, "command --debug")],
             [val(0, "true --inline=yes inpfile")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "exec")],
             [val(8, "command --debug=true --inline=yes inpfile")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "#")],
             [val(0, "This is=my comment style =#"),
              val(0, "")
             ]),
        eqil([key(0, "key"), key(2,"op"), key(4, "build")],
             [val(6, "help = build operation for op"),
              val(6, "exec ="),
              val(8, "command --debug=true --inline=yes inpfile"),
              val(6, "#= This is=my comment style =#"),
              val(0, "")
             ]),
        eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "help")],
             [val(0, "run me")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "exec")],
             [val(0, "run it")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "run")],
             [val(6, "help = run me"),
              val(6, "exec = run it")
             ]),
        eqil([key(0, "key"), key(2,"op")],
             [val(4, "build ="),
              val(6, "help = build operation for op"),
              val(6, "exec ="),
              val(8, "command --debug=true --inline=yes inpfile"),
              val(6, "#= This is=my comment style =#"),
              val(0, ""),
              val(4, "run ="),
              val(6, "help = run me"),
              val(6, "exec = run it")
             ]),
        eqil([key(0, "key")],
             [val(2, "op ="),
              val(4, "build ="),
              val(6, "help = build operation for op"),
              val(6, "exec ="),
              val(8, "command --debug=true --inline=yes inpfile"),
              val(6, "#= This is=my comment style =#"),
              val(0, ""),
              val(4, "run ="),
              val(6, "help = run me"),
              val(6, "exec = run it")
             ])
    ],
    ReParsed = [
        eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "help")],
             [val(0, "build operation for op")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "build"),
              key(6, "exec"), key(8, "command --debug")],
             [val(0, "true --inline=yes inpfile")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "exec")],
             [val(8, "command --debug = true --inline=yes inpfile")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "#")],
             [val(0, "This is=my comment style =#"),
              val(0, "")
             ]),
        eqil([key(0, "key"), key(2,"op"), key(4, "build")],
             [val(6, "help = build operation for op"),
              val(6, "exec ="),
              val(8, "command --debug = true --inline=yes inpfile"),
              val(6, "# = This is=my comment style =#"),
              val(0, "")
             ]),
        eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "help")],
             [val(0, "run me")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "exec")],
             [val(0, "run it")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "run")],
             [val(6, "help = run me"),
              val(6, "exec = run it")
             ]),
        eqil([key(0, "key"), key(2,"op")],
             [val(4, "build ="),
              val(6, "help = build operation for op"),
              val(6, "exec ="),
              val(8, "command --debug = true --inline=yes inpfile"),
              val(6, "# = This is=my comment style =#"),
              val(0, ""),
              val(4, "run ="),
              val(6, "help = run me"),
              val(6, "exec = run it")
             ]),
        eqil([key(0, "key")],
             [val(2, "op ="),
              val(4, "build ="),
              val(6, "help = build operation for op"),
              val(6, "exec ="),
              val(8, "command --debug = true --inline=yes inpfile"),
              val(6, "# = This is=my comment style =#"),
              val(0, ""),
              val(4, "run ="),
              val(6, "help = run me"),
              val(6, "exec = run it")
             ])
    ],
    check(Inp, Parsed, Out, already_normalized, ReParsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:key(key, op)),
    assertion(eng:key(key, op, build)),
    assertion(eng:key(key, op, build, help)),
    assertion(eng:key(key, op, build, exec)),
    assertion(eng:key(key, op, build, '#')),
    assertion(eng:eng(key, op, build, help, "build operation for op")),
    assertion(eng:eng(key, op, build, exec, "command --debug = true --inline=yes inpfile")),
    assertion(eng:eng(key, op, build, '#', "This is=my comment style =#")),
    assertion(eng:key(key, op, run)),
    assertion(eng:key(key, op, run, help)),
    assertion(eng:key(key, op, run, exec)),
    assertion(eng:eng(key, op, run, help, "run me")),
    assertion(eng:eng(key, op, run, exec, "run it")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key ]),
    findall((K1,K2), eng:key(K1,K2), K2S),
    assertion(K2S == [ (key,op) ]),
    findall((K1,K2,K3), eng:key(K1,K2,K3), K3S),
    assertion(K3S == [ (key,op,build), (key,op,run) ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(block_values, [nondet]) :-
    Inp = {|string||
| key =
|   op =
|     build =
|       help = build operation for op
|       exec = |
|         command --debug=true --inline=yes inpfile
|
|         command2 --out=free | cat -n
|       #= This is=my comment style =#
|
|     run =
|       help = run me
|       exec = | run it
|       one = |
|       two = |
|         second=#2
|       three
|         third=#3
|},
    % spaces around =, adds an extra blank line for %3 gen_eqil_combine
    Out = {|string||
| key =
|   op =
|     build =
|       help = build operation for op
|       exec = |
|         command --debug=true --inline=yes inpfile
|
|         command2 --out=free | cat -n
|       # = This is=my comment style =#
|
|     run =
|       help = run me
|       exec = | run it
|       one =
|       two = |
|         second=#2
|       three =
|         third = #3
|},
    E0 = eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "help")],
              [val(0, "build operation for op")]),
    E1 = eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "exec")],
              [val(8, "command --debug=true --inline=yes inpfile"),
               val(0, ""),
               val(8, "command2 --out=free | cat -n")
              ]),
    E3 = eqil([key(0, "key"), key(2,"op"), key(4, "build")],
              [val(6, "help = build operation for op"),
               val(6, "exec ="),
               val(8, "command --debug=true --inline=yes inpfile"),
               val(0, ""),
               val(8, "command2 --out=free | cat -n"),
               val(6, "#= This is=my comment style =#"),
               val(0, "")
              ]),
    E2 = eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "#")],
              [val(0, "This is=my comment style =#"),
               val(0, "")
              ]),
    E4 = eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "help")],
              [val(0, "run me")]),
    E5 = eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "exec")],
              [val(0, "| run it")]),
    E6 = eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "one")], []),
    E7 = eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "two")],
              [val(8, "second=#2")]),
    E8 = eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "three"),
               key(8, "third")],
              [val(0, "#3")]),
    E9 = eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "three")],
              [val(8, "third=#3")]),
    E10 = eqil([key(0, "key"), key(2,"op"), key(4, "run")],
               [val(6, "help = run me"),
                val(6, "exec = | run it"),
                val(6, "one ="),
                val(6, "two ="),
                val(8, "second=#2"),
                val(6, "three"),
                val(8, "third=#3")
               ]),
    E11 = eqil([key(0, "key"), key(2,"op")],
               [val(4, "build ="),
                val(6, "help = build operation for op"),
                val(6, "exec ="),
                val(8, "command --debug=true --inline=yes inpfile"),
                val(0, ""),
                val(8, "command2 --out=free | cat -n"),
                val(6, "#= This is=my comment style =#"),
                val(0, ""),
                val(4, "run ="),
                val(6, "help = run me"),
                val(6, "exec = | run it"),
                val(6, "one ="),
                val(6, "two ="),
                val(8, "second=#2"),
                val(6, "three"),
                val(8, "third=#3")
               ]),
    E12 = eqil([key(0, "key")],
               [val(2, "op ="),
                val(4, "build ="),
                val(6, "help = build operation for op"),
                val(6, "exec ="),
                val(8, "command --debug=true --inline=yes inpfile"),
                val(0, ""),
                val(8, "command2 --out=free | cat -n"),
                val(6, "#= This is=my comment style =#"),
                val(0, ""),
                val(4, "run ="),
                val(6, "help = run me"),
                val(6, "exec = | run it"),
                val(6, "one ="),
                val(6, "two ="),
                val(8, "second=#2"),
                val(6, "three"),
                val(8, "third=#3")
               ]),
    Parsed = [ E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12 ],
    ReParsed = [
        E0, E1, E2,
        eqil([key(0, "key"), key(2,"op"), key(4, "build")],
             [val(6, "help = build operation for op"),
              val(6, "exec ="),
              val(8, "command --debug=true --inline=yes inpfile"),
              val(0, ""),
              val(8, "command2 --out=free | cat -n"),
              val(6, "# = This is=my comment style =#"),
              val(0, "")
             ]),
        E4, E5, E6, E7, E8,
        eqil([key(0, "key"), key(2,"op"), key(4, "run"), key(6, "three")],
             [val(8, "third = #3")]),
        eqil([key(0, "key"), key(2,"op"), key(4, "run")],
               [val(6, "help = run me"),
                val(6, "exec = | run it"),
                val(6, "one ="),
                val(6, "two ="),
                val(8, "second=#2"),
                val(6, "three ="),
                val(8, "third = #3")
               ]),
        eqil([key(0, "key"), key(2,"op")],
             [val(4, "build ="),
              val(6, "help = build operation for op"),
              val(6, "exec ="),
              val(8, "command --debug=true --inline=yes inpfile"),
              val(0, ""),
              val(8, "command2 --out=free | cat -n") ,
              val(6, "# = This is=my comment style =#"),
              val(0, ""),
              val(4, "run ="),
              val(6, "help = run me"),
              val(6, "exec = | run it"),
              val(6, "one ="),
              val(6, "two ="),
              val(8, "second=#2"),
              val(6, "three ="),
              val(8, "third = #3")
             ]),
        eqil([key(0, "key")],
             [val(2, "op ="),
              val(4, "build ="),
              val(6, "help = build operation for op"),
              val(6, "exec ="),
              val(8, "command --debug=true --inline=yes inpfile"),
              val(0, ""),
              val(8, "command2 --out=free | cat -n"),
              val(6, "# = This is=my comment style =#"),
              val(0, ""),
              val(4, "run ="),
              val(6, "help = run me"),
              val(6, "exec = | run it"),
              val(6, "one ="),
              val(6, "two ="),
              val(8, "second=#2"),
              val(6, "three ="),
              val(8, "third = #3")
             ])
    ],
    check(Inp, Parsed, Out, already_normalized, ReParsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:key(key, op)),
    assertion(eng:key(key, op, build)),
    assertion(eng:key(key, op, build, help)),
    assertion(eng:key(key, op, build, exec)),
    assertion(eng:key(key, op, build, '#')),
    assertion(eng:eng(key, op, build, help, "build operation for op")),
    assertion(eng:eng(key, op, build, exec, "command --debug=true --inline=yes inpfile\n\ncommand2 --out=free | cat -n")),
    assertion(eng:eng(key, op, build, '#', "This is=my comment style =#")),
    assertion(eng:key(key, op, run)),
    assertion(eng:key(key, op, run, help)),
    assertion(eng:key(key, op, run, exec)),
    assertion(eng:eng(key, op, run, help, "run me")),
    assertion(eng:eng(key, op, run, exec, "| run it")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key ]),
    findall((K1,K2), eng:key(K1,K2), K2S),
    assertion(K2S == [ (key,op) ]),
    findall((K1,K2,K3), eng:key(K1,K2,K3), K3S),
    assertion(K3S == [ (key,op,build), (key,op,run) ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(blank_key_and_values, [nondet]) :-
    Inp = {|string||
| key =
|   =
|     foo =
|       bar
| key
|   =
|     cow =
|       = moo
|       = graze
|
| = blank
| mid = point
| = another blank
| =
| end =
|  here
|},
    % spaces around =, adds an extra blank line for %3 gen_eqil_combine
    Out = {|string||
| key =
|   key1 =
|     foo =
|       bar
|   key4 =
|     cow =
|       cow2 = moo
|       cow3 = graze
|
| 5 = blank
| mid = point
| 6 = another blank
| 7 =
| end =
|  here
|},
    E0 = eqil([key(0, "key"), key(2, ""), key(4, "foo")],
              [val(6, "bar")
              ]),
    E1 = eqil([key(0, "key"), key(2, "")],
              [val(4, "foo ="),
               val(6, "bar")
              ]),
    E2 = eqil([key(0, "key")],
              [val(2, ""),
               val(4, "foo ="),
               val(6, "bar")
              ]),
    E3 = eqil([key(0, "key"), key(2, ""), key(4, "cow"), key(6, "")],
              [val(0, "moo")
              ]),
    E4 = eqil([key(0, "key"), key(2, ""), key(4, "cow"), key(6, "")],
              [val(0, "graze"),
               val(0, "")
              ]),
    E5 = eqil([key(0, "key"), key(2, ""), key(4, "cow")],
              [val(6, "= moo"),
               val(6, "= graze"),
               val(0, "")
              ]),
    E6 = eqil([key(0, "key"), key(2, "")],
              [val(4, "cow ="),
               val(6, "= moo"),
               val(6, "= graze"),
               val(0, "")
              ]),
    E7 = eqil([key(0, "key")],
              [val(2, ""),
               val(4, "cow ="),
               val(6, "= moo"),
               val(6, "= graze"),
               val(0, "")
              ]),
    E8 = eqil([key(0, "")], [val(0, "blank")]),
    E9 = eqil([key(0, "mid")], [val(0,"point")]),
    E10 = eqil([key(0, "")], [val(0,"another blank")]),
    E11 = eqil([key(0, "")], []),
    E12 = eqil([key(0, "end")], [val(1,"here")]),
    Parsed = [ E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12 ],
    Normalized = [
        % E0 with assigned key:
        eqil([key(0, "key"), key(2, "key1"), key(4, "foo")],
             [val(6, "bar")
              ]),
        % E1 with assigned key:
        eqil([key(0, "key"), key(2, "key1")],
              [val(4, "foo ="),
               val(6, "bar")
              ]),
        % E2 removed,
        % E3 with assigned key:
        eqil([key(0, "key"), key(2, "key4"), key(4, "cow"), key(6, "cow2")],
              [val(0, "moo")
              ]),
        % E4 with assigned key
        eqil([key(0, "key"), key(2, "key4"), key(4, "cow"), key(6, "cow3")],
             [val(0, "graze"),
              val(0, "")
             ]),
        % E5 with assigned key:
        eqil([key(0, "key"), key(2, "key4"), key(4, "cow")],
              [val(6, "= moo"),
               val(6, "= graze"),
               val(0, "")
              ]),
        % E6 with assigned key:
        eqil([key(0, "key"), key(2, "key4")],
              [val(4, "cow ="),
               val(6, "= moo"),
               val(6, "= graze"),
               val(0, "")
              ]),
        % E7 removed.
        % E8 with assigned key:
        eqil([key(0, "5")], [val(0, "blank")]),
        E9,
        % E10 with assigned key:
        eqil([key(0, "6")], [val(0,"another blank")]),
        % E11 with assigned key:
        eqil([key(0, "7")], []),
        E12 ],
    ReParsed = [
        % E0 with assigned key:
        eqil([key(0, "key"), key(2, "key1"), key(4, "foo")],
             [val(6, "bar")
              ]),
        % E1 with assigned key:
        eqil([key(0, "key"), key(2, "key1")],
              [val(4, "foo ="),
               val(6, "bar")
              ]),
        % E2 removed,
        % E3 with assigned key:
        eqil([key(0, "key"), key(2, "key4"), key(4, "cow"), key(6, "cow2")],
              [val(0, "moo")
              ]),
        % E4 with assigned key
        eqil([key(0, "key"), key(2, "key4"), key(4, "cow"), key(6, "cow3")],
             [val(0, "graze"),
              val(0, "")
             ]),
        % E5 with assigned key and value updates for assigned keys:
        eqil([key(0, "key"), key(2, "key4"), key(4, "cow")],
              [val(6, "cow2 = moo"),
               val(6, "cow3 = graze"),
               val(0, "")
              ]),
        % E6 with assigned key and value updates for assigned keys:
        eqil([key(0, "key"), key(2, "key4")],
              [val(4, "cow ="),
               val(6, "cow2 = moo"),
               val(6, "cow3 = graze"),
               val(0, "")
              ]),
        % E2 + E7 with value updates for assigned keys:
        eqil([key(0, "key")],
              [val(2, "key1 ="),
               val(4, "foo ="),
               val(6, "bar"),
               val(2, "key4 ="),
               val(4, "cow ="),
               val(6, "cow2 = moo"),
               val(6, "cow3 = graze"),
               val(0, "")
              ]),
        % E8 with assigned key:
        eqil([key(0, "5")], [val(0, "blank")]),
        E9,
        % E10 with assigned key:
        eqil([key(0, "6")], [val(0,"another blank")]),
        % E11 with assigned key:
        eqil([key(0, "7")], []),
        E12 ],
    check(Inp, Parsed, Out, Normalized, ReParsed, _Result).


test(duplicate_keys, [nondet]) :-
    Inp = {|string||
| key =
|   op = build
|   op = test
|   op = run
|   = right
|   = center
|     of things
|   = left
|},
    % generated keys for blank keys
    Out = {|string||
| key =
|   op = build
|        test
|        run
|   key1 = right
|   key2 = center
|     of things
|   key3 = left
|},
    Parsed = [
        eqil([key(0, "key"), key(2,"op")], [val(0, "build")]),
        eqil([key(0, "key"), key(2,"op")], [val(0, "test")]),
        eqil([key(0, "key"), key(2,"op")], [val(0, "run")]),
        eqil([key(0, "key"), key(2,"")], [val(0, "right")]),
        eqil([key(0, "key"), key(2,"")], [val(0, "center"),
                                          val(4, "of things")]),
        eqil([key(0, "key"), key(2,"")], [val(0, "left")]),
        eqil([key(0, "key")],
             [val(2, "op = build"),
              val(2, "op = test"),
              val(2, "op = run"),
              val(2, "= right"),
              val(2, "= center"),
              val(4, "of things"),
              val(2, "= left")
             ])
    ],
    Normalized = [
        eqil([key(0, "key"), key(2,"op")], [val(0, "build"),
                                            val(7, "test"),
                                            val(7, "run")]),
        eqil([key(0, "key"), key(2, "key1")], [val(0, "right")]),
        eqil([key(0, "key"), key(2, "key2")], [val(0, "center"),
                                               val(4, "of things")]),
        eqil([key(0, "key"), key(2, "key3")], [val(0, "left")])
        %% eqil([key(0, "key")],
        %%      [val(2, "op = build"),
        %%       val(2, "op = test"),
        %%       val(2, "op = run"),
        %%       val(2, "= right"),
        %%       val(2, "= center"),
        %%       val(4, "of things"),
        %%       val(2, "= left")
        %%      ])
    ],
    ReParsed = [
        eqil([key(0, "key"), key(2,"op")], [val(0, "build"),
                                            val(7, "test"),
                                            val(7, "run")
                                           ]),
        %% eqil([key(0, "key"), key(2,"op")], [val(0, "test")]),
        %% eqil([key(0, "key"), key(2,"op")], [val(0, "run")]),
        eqil([key(0, "key"), key(2, "key1")], [val(0, "right")]),
        eqil([key(0, "key"), key(2, "key2")], [val(0, "center"),
                                               val(4, "of things")]),
        eqil([key(0, "key"), key(2, "key3")], [val(0, "left")]),
        eqil([key(0, "key")],
             [val(2, "op = build"),
              val(7, "test"),
              val(7, "run"),
              val(2, "key1 = right"),
              val(2, "key2 = center"),
              val(4, "of things"),
              val(2, "key3 = left")
             ])
    ],
    check(Inp, Parsed, Out, Normalized, ReParsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(key)),
    assertion(eng:key(key, op)),
    assertion(eng:eng(key, op, "build\ntest\nrun")),
    %% assertion(eng:eng(key, op, "test")),
    %% assertion(eng:eng(key, op, "run")),
    assertion(eng:key(key, 'key1')),
    assertion(eng:key(key, 'key2')),
    assertion(eng:key(key, 'key3')),
    assertion(eng:eng(key, 'key3', "left")),
    assertion(eng:eng(key, 'key2', "center\nof things")),
    assertion(eng:eng(key, 'key1', "right")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key ]),
    findall((K1,K2), eng:key(K1,K2), K2S),
    assertion(K2S == [ (key,op), (key,'key1'), (key,'key2'), (key,'key3') ]),
    findall((K1,K2,K3), eng:key(K1,K2,K3), K3S),
    assertion(K3S == []),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.


test(top_level_implicit_keys, [nondet]) :-
    Inp = {|string||
| system
|   spec
|     This is my Specification =
|       name = foo
|       file = foo.spec
|   spec
|     Another spec = here
|       name = 'nuther
|       'not = used
|
|},
    % all keys have =, normalizing combines keys, keys cannot have values and
    % subkeys
    Out = {|string||
| system =
|   spec =
|     This is my Specification =
|       name = foo
|       file = foo.spec
|     Another spec =
|       name = 'nuther
|       'not = used
|
|},
    Parsed = [
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"), key(6, "name")],
             [val(0, "foo")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"), key(6, "file")],
             [val(0, "foo.spec")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification")],
             [val(6, "name = foo"),
              val(6, "file = foo.spec")
             ]),
        eqil([key(0, "system"), key(2, "spec")],
             [val(4, "This is my Specification ="),
              val(6, "name = foo"),
              val(6, "file = foo.spec")
             ]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec"), key(6, "name")],
             [val(0, "'nuther")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec"), key(6, "'not")],
             [val(0, "used")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec")],
             [val(0, "here"),
              val(6, "name = 'nuther"),
              val(6, "'not = used")
             ]),
        eqil([key(0, "system"), key(2, "spec")],
             [val(4, "Another spec = here"),
              val(6, "name = 'nuther"),
              val(6, "'not = used")
             ]),
        eqil([key(0, "system")],
             [val(2, "spec"),
              val(4, "This is my Specification ="),
              val(6, "name = foo"),
              val(6, "file = foo.spec"),
              val(2, "spec"),
              val(4, "Another spec = here"),
              val(6, "name = 'nuther"),
              val(6, "'not = used")
             ])
    ],
    % Combine identical keys
    Normalized = [
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"), key(6, "name")],
             [val(0, "foo")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"), key(6, "file")],
             [val(0, "foo.spec")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification")],
             [val(6, "name = foo"),
              val(6, "file = foo.spec")
             ]),
        eqil([key(0, "system"), key(2, "spec")],
             [val(4, "This is my Specification ="),
              val(6, "name = foo"),
              val(6, "file = foo.spec"),
              % Added:
              val(4, "Another spec = here"),
              val(6, "name = 'nuther"),
              val(6, "'not = used")
             ]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec"), key(6, "name")],
             [val(0, "'nuther")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec"), key(6, "'not")],
             [val(0, "used")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec")],
             [val(0, "here"),
              val(6, "name = 'nuther"),
              val(6, "'not = used")
             ]),
        eqil([key(0, "system")],
             [val(2, "spec"),
              val(4, "This is my Specification ="),
              val(6, "name = foo"),
              val(6, "file = foo.spec"),
              val(2, "spec"),
              val(4, "Another spec = here"),
              val(6, "name = 'nuther"),
              val(6, "'not = used")
             ])
    ],
    ReParsed = [
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"), key(6, "name")],
             [val(0, "foo")]),
        %% Moved vvvv
        %% eqil([key(0, "system"), key(2, "spec")],
        %%      [val(4, "This is my Specification ="),
        %%       val(6, "name = foo"),
        %%       val(6, "file = foo.spec"),
        %%       % Added:
        %%       val(4, "Another spec ="),
        %%       val(6, "name = 'nuther"),
        %%       val(6, "'not = used")
        %%      ]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"), key(6, "file")],
             [val(0, "foo.spec")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification")],
             [val(6, "name = foo"),
              val(6, "file = foo.spec")
             ]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec"), key(6, "name")],
             [val(0, "'nuther")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec"), key(6, "'not")],
             [val(0, "used")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec")],
             [%% val(0, "here"),
              val(6, "name = 'nuther"),
              val(6, "'not = used")
             ]),
        % Moved here:
        eqil([key(0, "system"), key(2, "spec")],
             [val(4, "This is my Specification ="),
              val(6, "name = foo"),
              val(6, "file = foo.spec"),
              % Added:
              val(4, "Another spec ="),
              val(6, "name = 'nuther"),
              val(6, "'not = used")
             ]),
        eqil([key(0, "system")],
             [val(2, "spec ="),
              val(4, "This is my Specification ="),
              val(6, "name = foo"),
              val(6, "file = foo.spec"),
              %% val(2, "spec"),
              val(4, "Another spec ="),
              val(6, "name = 'nuther"),
              val(6, "'not = used")
             ])
    ],
    check(Inp, Parsed, Out, Normalized, ReParsed, Result),
    revert_assert_eng,
    assert_eqil(Result, _),
    assertion(eng:key(system)),
    assertion(eng:key(system, spec)),
    assertion(eng:key(system, spec, 'This is my Specification')),
    assertion(eng:key(system, spec, 'This is my Specification', name)),
    assertion(eng:key(system, spec, 'This is my Specification', file)),
    assertion(eng:eng(system, spec, 'This is my Specification', name, "foo")),
    assertion(eng:eng(system, spec, 'This is my Specification', file, "foo.spec")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ system ]),
    findall((K1,K2), eng:key(K1,K2), K2S),
    assertion(K2S == [ (system,spec) ]),
    findall((K1,K2,K3), eng:key(K1,K2,K3), K3S),
    assertion(K3S == [ (system,spec,'This is my Specification'),
                       (system,spec,'Another spec')
                     ]),
    %% findall((K,V), eng:eng(K,,V), KVS),
    %% writeln(KVS)
    revert_assert_eng.

test(blank_keys, [nondet]) :-
    Inp = {|string||
| system
|   spec
|     This is my Specification =
|       = foo
|       = foo.spec
|       = used
|       = at this time
|   spec
|     Another spec = here
|       = there
|
|},
    % All keys have =, keys cannot have values and sub-keys, similar keys
    % combined
    Out = {|string||
| system =
|   spec =
|     This is my Specification =
|       This is my Specification1 = foo
|       This is my Specification2 = foo.spec
|       This is my Specification3 = used
|       This is my Specification4 = at this time
|     Another spec =
|       Another spec5 = there
|
|},
    Parsed = [
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"), key(6, "")],
             [val(0, "foo")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"), key(6, "")],
             [val(0, "foo.spec")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"), key(6, "")],
             [val(0, "used")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"), key(6, "")],
             [val(0, "at this time")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification")],
             [val(6, "= foo"),
              val(6, "= foo.spec"),
              val(6, "= used"),
              val(6, "= at this time")
             ]),
        eqil([key(0, "system"), key(2, "spec")],
             [val(4, "This is my Specification ="),
              val(6, "= foo"),
              val(6, "= foo.spec"),
              val(6, "= used"),
              val(6, "= at this time")
             ]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec"), key(6, "")],
             [val(0, "there")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec")],
             [val(0, "here"),
              val(6, "= there")]),
        eqil([key(0, "system"), key(2, "spec")],
             [val(4, "Another spec = here"),
              val(6, "= there")]),
        eqil([key(0, "system")],
             [val(2, "spec"),
              val(4, "This is my Specification ="),
              val(6, "= foo"),
              val(6, "= foo.spec"),
              val(6, "= used"),
              val(6, "= at this time"),
                      val(2, "spec"),
                      val(4, "Another spec = here"),
                      val(6, "= there")])
    ],
    Normalized = [
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"),
              key(6, "This is my Specification1")],
             [val(0, "foo")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"),
              key(6, "This is my Specification2")],
             [val(0, "foo.spec")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"),
              key(6, "This is my Specification3")],
             [val(0, "used")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"),
              key(6, "This is my Specification4")],
             [val(0, "at this time")]),
        %% eqil([key(0, "system"), key(2, "spec")],
        %%      [val(4, "This is my Specification ="),
        %%       val(6, "= foo"),
        %%       val(6, "= foo.spec"),
        %%       val(6, "= used"),
        %%       val(6, "= at this time"),
        %%       val(4, "Another spec = here"),
        %%       val(6, "= there")
        %%      ]),
        %% eqil([key(0, "system"), key(2, "spec"),
        %%       key(4, "This is my Specification")],
        %%      [val(6, "= foo"),
        %%       val(6, "= foo.spec"),
        %%       val(6, "= used"),
        %%       val(6, "= at this time")
        %%      ]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec"),
              key(6, "Another spec5")],
             [val(0, "there")])
        %% eqil([key(0, "system"), key(2, "spec"),
        %%       key(4, "Another spec")],
        %%      [val(0, "here"),
        %%       val(6, "= there")]),
        %% eqil([key(0, "system")],
        %%      [val(2, "spec"),
        %%       val(4, "This is my Specification ="),
        %%       val(6, "= foo"),
        %%       val(6, "= foo.spec"),
                %%       val(6, "= used"),
        %%       val(6, "= at this time"),
        %%       val(2, "spec"),
        %%       val(4, "Another spec = here"),
        %%       val(6, "= there")])
    ],
    ReParsed = [
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"),
              key(6, "This is my Specification1")],
             [val(0, "foo")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"),
              key(6, "This is my Specification2")],
             [val(0, "foo.spec")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"),
              key(6, "This is my Specification3")],
             [val(0, "used")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification"),
              key(6, "This is my Specification4")],
             [val(0, "at this time")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "This is my Specification")],
             [val(6, "This is my Specification1 = foo"),
              val(6, "This is my Specification2 = foo.spec"),
              val(6, "This is my Specification3 = used"),
              val(6, "This is my Specification4 = at this time")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec"),
              key(6, "Another spec5")],
             [val(0, "there")]),
        eqil([key(0, "system"), key(2, "spec"),
              key(4, "Another spec")],
             [val(6, "Another spec5 = there")]),
        eqil([key(0, "system"), key(2, "spec")],
             [val(4, "This is my Specification ="),
              val(6, "This is my Specification1 = foo"),
              val(6, "This is my Specification2 = foo.spec"),
              val(6, "This is my Specification3 = used"),
              val(6, "This is my Specification4 = at this time"),
              val(4, "Another spec ="),
              val(6, "Another spec5 = there")
             ]),
        eqil([key(0, "system")],
             [val(2, "spec ="),
              val(4, "This is my Specification ="),
              val(6, "This is my Specification1 = foo"),
              val(6, "This is my Specification2 = foo.spec"),
              val(6, "This is my Specification3 = used"),
              val(6, "This is my Specification4 = at this time"),
              val(4, "Another spec ="),
              val(6, "Another spec5 = there")
             ])
    ],
    check(Inp, Parsed, Out, Normalized, ReParsed, _Result).

test(adjacent_keys, [nondet]) :-
    Inp = {|string||
| key1
|   key2 = value2
| key1
|   key3 = value3
|},
    Parsed = [
        eqil([key(0, "key1"), key(2, "key2")],
             [val(0, "value2")]),
        eqil([key(0, "key1")],
             [val(2, "key2 = value2")]),
        eqil([key(0, "key1"), key(2, "key3")],
             [val(0, "value3")]),
        eqil([key(0, "key1")],
             [val(2, "key3 = value3")])
    ],
    % Similar keys joined, all keys have =
    Normalized = [
        eqil([key(0, "key1"), key(2, "key2")],
             [val(0, "value2")]),
        eqil([key(0, "key1")],
             [val(2, "key2 = value2"),
              val(2, "key3 = value3")]),
        eqil([key(0, "key1"), key(2, "key3")],
             [val(0, "value3")])
    ],
    NormText = {|string||
| key1 =
|   key2 = value2
|   key3 = value3
|},
    ReParsed = [
        eqil([key(0, "key1"), key(2, "key2")],
             [val(0, "value2")]),
        eqil([key(0, "key1"), key(2, "key3")],
             [val(0, "value3")]),
        eqil([key(0, "key1")],
             [val(2, "key2 = value2"),
              val(2, "key3 = value3")])
    ],
    check(Inp, Parsed, NormText, Normalized, ReParsed, _Result).

test(mixed_keys, [nondet]) :-
    % Note that there are spaces on the lines between k7 and end (warning, don't
    % let the editor trim trailing whitespace and remove these!
    Inp = {|string||
| key1 =
|   key2 =
|     key3 = value3
|     key4 =
|
|       = value4.1
|
|       = value4.2
|
|       = value4.3
|
|     key5 = value5
|
|     k6 =
|       k6sub = foo
|               bar
|
|     k7 = value7
|    
|     
| end = here
|},
    Parsed = [
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key3")],
             [val(0, "value3")]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4"), key(6,"")],
             [val(0, "value4.1"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4"), key(6, "")],
             [val(0, "value4.2"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4"), key(6, "")],
             [val(0, "value4.3"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4")],
             [val(0, ""),
              val(6, "= value4.1"),
              val(0, ""),
              val(6, "= value4.2"),
              val(0, ""),
              val(6, "= value4.3"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key5")],
             [val(0, "value5"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "k6"), key(6, "k6sub")],
             [val(0, "foo"),
              val(14, "bar"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "k6")],
             [val(6, "k6sub = foo"),
              val(14, "bar"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "k7")],
             [val(0, "value7"),
              val(3, ""),
              val(4, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2")],
             [val(4, "key3 = value3"),
              val(4, "key4 ="),
              val(0, ""),
              val(6, "= value4.1"),
              val(0, ""),
              val(6, "= value4.2"),
              val(0, ""),
              val(6, "= value4.3"),
              val(0, ""),
              val(4, "key5 = value5"),
              val(0, ""),
              val(4, "k6 ="),
              val(6, "k6sub = foo"),
              val(14, "bar"),
              val(0, ""),
              val(4, "k7 = value7"),
              val(3, ""),
              val(4, "")
             ]),
        eqil([key(0, "key1")],
             [val(2, "key2 ="),
              val(4, "key3 = value3"),
              val(4, "key4 ="),
              val(0, ""),
              val(6, "= value4.1"),
              val(0, ""),
              val(6, "= value4.2"),
              val(0, ""),
              val(6, "= value4.3"),
              val(0, ""),
              val(4, "key5 = value5"),
              val(0, ""),
              val(4, "k6 ="),
              val(6, "k6sub = foo"),
              val(14, "bar"),
              val(0, ""),
              val(4, "k7 = value7"),
              val(3, ""),
              val(4, "")
             ]),
        eqil([key(0, "end")], [val(0, "here")])
    ],
    % Similar keys joined, all keys have =, blank keys autogen
    Out = {|string||
| key1 =
|   key2 =
|     key3 = value3
|     key4 =
|       key41 = value4.1
|
|       key42 = value4.2
|
|       key43 = value4.3
|
|     key5 = value5
|
|     k6 =
|       k6sub = foo
|               bar
|
|     k7 = value7
|    
|     
| end = here
|},
    Normalized = [
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key3")],
             [val(0, "value3")]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4"), key(6, "key41")],
             [val(0, "value4.1"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4"), key(6, "key42")],
             [val(0, "value4.2"),
              val(0, "")
             ]),
        %% eqil([key(0, "key1"), key(2, "key2"), key(4, "key4")],
        %%      [val(0, ""),
        %%       val(6, "= value4.1"),
        %%       val(0, ""),
        %%       val(6, "= value4.2"),
        %%       val(0, ""),
        %%       val(6, "= value4.3"),
        %%       val(0, "")
        %%      ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4"), key(6, "key43")],
             [val(0, "value4.3"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key5")],
             [val(0, "value5"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "k6"), key(6, "k6sub")],
             [val(0, "foo"),
              val(14, "bar"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "k6")],
             [val(6, "k6sub = foo"),
              val(14, "bar"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "k7")],
             [val(0, "value7"),
              val(3, ""),
              val(4, "")
             ]),
        %% eqil([key(0, "key1"), key(2, "key2")],
        %%      [val(4, "key3 = value3"),
        %%       val(4, "key4 ="),
        %%       val(0, ""),
        %%       val(6, "= value4.1"),
        %%       val(0, ""),
        %%       val(6, "= value4.2"),
        %%       val(0, ""),
        %%       val(6, "= value4.3"),
        %%       val(0, ""),
        %%       val(4, "key5 = value5"),
        %%       val(0, ""),
        %%       val(4, "k6 ="),
        %%       val(6, "k6sub = foo"),
        %%       val(14, "bar"),
        %%       val(0, ""),
        %%       val(4, "k7 = value7")
        %%      ])
        %% eqil([key(0, "key1")],
        %%      [val(2, "key2 ="),
        %%       val(4, "key3 = value3"),
        %%       val(4, "key4 ="),
        %%       val(0, ""),
        %%       val(6, "= value4.1"),
        %%       val(0, ""),
        %%       val(6, "= value4.2"),
        %%       val(0, ""),
        %%       val(6, "= value4.3"),
        %%       val(0, ""),
        %%       val(4, "key5 = value5"),
        %%       val(0, ""),
        %%       val(4, "k6 ="),
        %%       val(6, "k6sub = foo"),
        %%       val(14, "bar"),
        %%       val(0, ""),
        %%       val(4, "k7 = value7")
        %%      ])
        eqil([key(0, "end")], [val(0, "here")])
    ],
    ReParsed = [
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key3")],
             [val(0, "value3")]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4"), key(6, "key41")],
             [val(0, "value4.1"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4"), key(6, "key42")],
             [val(0, "value4.2"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4"), key(6, "key43")],
             [val(0, "value4.3"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key4")],
             [%% val(0, ""),
              val(6, "key41 = value4.1"),
              val(0, ""),
              val(6, "key42 = value4.2"),
              val(0, ""),
              val(6, "key43 = value4.3"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "key5")],
             [val(0, "value5"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "k6"), key(6, "k6sub")],
             [val(0, "foo"),
              val(14, "bar"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "k6")],
             [val(6, "k6sub = foo"),
              val(14, "bar"),
              val(0, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2"), key(4, "k7")],
             [val(0, "value7"),
              val(3, ""),
              val(4, "")
             ]),
        eqil([key(0, "key1"), key(2, "key2")],
             [val(4, "key3 = value3"),
              val(4, "key4 ="),
              val(6, "key41 = value4.1"),
              val(0, ""),
              val(6, "key42 = value4.2"),
              val(0, ""),
              val(6, "key43 = value4.3"),
              val(0, ""),
              val(4, "key5 = value5"),
              val(0, ""),
              val(4, "k6 ="),
              val(6, "k6sub = foo"),
              val(14, "bar"),
              val(0, ""),
              val(4, "k7 = value7"),
              val(3, ""),
              val(4, "")
             ]),
        eqil([key(0, "key1")],
             [val(2, "key2 ="),
              val(4, "key3 = value3"),
              val(4, "key4 ="),
              %% val(0, ""),
              val(6, "key41 = value4.1"),
              val(0, ""),
              val(6, "key42 = value4.2"),
              val(0, ""),
              val(6, "key43 = value4.3"),
              val(0, ""),
              val(4, "key5 = value5"),
              val(0, ""),
              val(4, "k6 ="),
              val(6, "k6sub = foo"),
              val(14, "bar"),
              val(0, ""),
              val(4, "k7 = value7"),
              val(3, ""),
              val(4, "")
             ]),
        eqil([key(0, "end")], [val(0, "here")])
    ],
    check(Inp, Parsed, Out, Normalized, ReParsed, _Result).


% ----------------------------------------------------------------------
% Samples from spec

test(sample1, [nondet]) :-
    Inp = {|string||
|  foo
|    farm =
|      cow = moo=says hello
|      pig
|        = grunt
|        = oink
|  foo =
|    farm =
|      chicken = cluck
|      info =
|        A chicken is an animal (chicken=animal) but not all
|        animals are chickens, so we cannot say = for animal
|        and chicken.
|},
    % spaces around =, adds an extra blank line for %3 gen_eqil_combine.  Some
    % unexpected effects in the info value.
    Out = {|string||
|  foo =
|    farm =
|      cow = moo=says hello
|      pig =
|        pig1 = grunt
|        pig2 = oink
|      chicken = cluck
|      info =
|        A chicken is an animal (chicken = animal) but not all
|        animals are chickens, so we cannot say = for animal
|        and chicken. =
|},
    E1 = eqil([key(0, "foo"), key(2,"farm"), key(4, "cow")],
              [val(0, "moo=says hello")
              ]),
    E2 = eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "")],
              [val(0, "grunt")
              ]),
    E6 = eqil([key(0, "foo")],
              [val(2, "farm ="),
               val(4, "cow = moo=says hello"),
               val(4, "pig"),
               val(6, "= grunt"),
               val(6, "= oink")
              ]),
    E5 = eqil([key(0, "foo"), key(2,"farm")],
              [val(4, "cow = moo=says hello"),
               val(4, "pig"),
               val(6, "= grunt"),
               val(6, "= oink")
              ]),
    E4 = eqil([key(0, "foo"), key(2,"farm"), key(4, "pig")],
              [val(6, "= grunt"),
               val(6, "= oink")
              ]),
    E3 = eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "")],
              [val(0, "oink")
              ]),
    E7 = eqil([key(0, "foo"), key(2,"farm"), key(4, "chicken")],
              [val(0, "cluck")
              ]),
    E8 = eqil([key(0, "foo"), key(2,"farm"), key(4, "info"),
               key(6, "A chicken is an animal (chicken")],
              [val(0, "animal) but not all")
              ]),
    E9 = eqil([key(0, "foo"), key(2,"farm"), key(4, "info"),
               key(6, "animals are chickens, so we cannot say")],
              [val(0, "for animal")
              ]),
    E10 = eqil([key(0, "foo"), key(2,"farm"), key(4, "info"),
                key(6, "and chicken.")],
               []),
    E11 = eqil([key(0, "foo"), key(2,"farm"), key(4, "info")],
               [val(6, "A chicken is an animal (chicken=animal) but not all"),
                val(6, "animals are chickens, so we cannot say = for animal"),
                val(6, "and chicken.")
               ]),
    E12 = eqil([key(0, "foo"), key(2,"farm")],
               [val(4, "chicken = cluck"),
                val(4, "info ="),
                val(6, "A chicken is an animal (chicken=animal) but not all"),
                val(6, "animals are chickens, so we cannot say = for animal"),
                val(6, "and chicken.")
               ]),
    E13 = eqil([key(0, "foo")],
               [val(2,"farm ="),
                val(4, "chicken = cluck"),
                val(4, "info ="),
                val(6, "A chicken is an animal (chicken=animal) but not all"),
                val(6, "animals are chickens, so we cannot say = for animal"),
                val(6, "and chicken.")
               ]),
    Parsed = [
        E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13
    ],
    Normalized = [
        E1,
        eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "pig1")],
             [val(0, "grunt")
             ]),
        eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "pig2")],
             [val(0, "oink")
             ]),
        E7, E8, E9, E10, E11
    ],
    ReParsed = [
        E1,
        eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "pig1")],
             [val(0, "grunt")
             ]),
        eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "pig2")],
             [val(0, "oink")
             ]),
        eqil([key(0, "foo"), key(2,"farm"), key(4, "pig")],
             [val(6, "pig1 = grunt"),
              val(6, "pig2 = oink")
             ]),
        E7, E8, E9, E10,
        eqil([key(0, "foo"), key(2,"farm"), key(4, "info")],
             [val(6, "A chicken is an animal (chicken = animal) but not all"),
              val(6, "animals are chickens, so we cannot say = for animal"),
              val(6, "and chicken. =")
             ]),
        eqil([key(0, "foo"), key(2,"farm")],
             [val(4, "cow = moo=says hello"),
              val(4, "pig ="),
              val(6, "pig1 = grunt"),
              val(6, "pig2 = oink"),
              val(4, "chicken = cluck"),
              val(4, "info ="),
              val(6, "A chicken is an animal (chicken = animal) but not all"),
              val(6, "animals are chickens, so we cannot say = for animal"),
              val(6, "and chicken. =")
             ]),
        eqil([key(0, "foo")],
             [val(2,"farm ="),
              val(4, "cow = moo=says hello"),
              val(4, "pig ="),
              val(6, "pig1 = grunt"),
              val(6, "pig2 = oink"),
              val(4, "chicken = cluck"),
              val(4, "info ="),
              val(6, "A chicken is an animal (chicken = animal) but not all"),
              val(6, "animals are chickens, so we cannot say = for animal"),
              val(6, "and chicken. =")
             ])
    ],
    check(Inp, Parsed, Out, Normalized, ReParsed, _Result).

test(sample1_valblock, [nondet]) :-
    Inp = {|string||
|  foo
|    farm =
|      cow = moo=says hello
|      pig
|        = grunt
|        = oink
|  foo =
|    farm =
|      chicken = cluck
|      info = |
|        A chicken is an animal (chicken=animal) but not all
|        animals are chickens, so we cannot say = for animal
|        and chicken.
|},
    % spaces around =, adds an extra blank line for %3 gen_eqil_combine.  Some
    % unexpected effects in the info value.
    Out = {|string||
|  foo =
|    farm =
|      cow = moo=says hello
|      pig =
|        pig1 = grunt
|        pig2 = oink
|      chicken = cluck
|      info = |
|        A chicken is an animal (chicken=animal) but not all
|        animals are chickens, so we cannot say = for animal
|        and chicken.
|},
    E1 = eqil([key(0, "foo"), key(2,"farm"), key(4, "cow")],
              [val(0, "moo=says hello")
              ]),
    E2 = eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "")],
              [val(0, "grunt")
              ]),
    E6 = eqil([key(0, "foo")],
              [val(2, "farm ="),
               val(4, "cow = moo=says hello"),
               val(4, "pig"),
               val(6, "= grunt"),
               val(6, "= oink")
              ]),
    E5 = eqil([key(0, "foo"), key(2,"farm")],
              [val(4, "cow = moo=says hello"),
               val(4, "pig"),
               val(6, "= grunt"),
               val(6, "= oink")
              ]),
    E4 = eqil([key(0, "foo"), key(2,"farm"), key(4, "pig")],
              [val(6, "= grunt"),
               val(6, "= oink")
              ]),
    E3 = eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "")],
              [val(0, "oink")
              ]),
    E7 = eqil([key(0, "foo"), key(2,"farm"), key(4, "chicken")],
              [val(0, "cluck")
              ]),
    E11 = eqil([key(0, "foo"), key(2,"farm"), key(4, "info")],
               [val(6, "A chicken is an animal (chicken=animal) but not all"),
                val(6, "animals are chickens, so we cannot say = for animal"),
                val(6, "and chicken.")
               ]),
    E12 = eqil([key(0, "foo"), key(2,"farm")],
               [val(4, "chicken = cluck"),
                val(4, "info ="),
                val(6, "A chicken is an animal (chicken=animal) but not all"),
                val(6, "animals are chickens, so we cannot say = for animal"),
                val(6, "and chicken.")
               ]),
    E13 = eqil([key(0, "foo")],
               [val(2,"farm ="),
                val(4, "chicken = cluck"),
                val(4, "info ="),
                val(6, "A chicken is an animal (chicken=animal) but not all"),
                val(6, "animals are chickens, so we cannot say = for animal"),
                val(6, "and chicken.")
               ]),
    Parsed = [
        E1, E2, E3, E4, E5, E6, E7, E11, E12, E13
    ],
    Normalized = [
        E1,
        eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "pig1")],
             [val(0, "grunt")
             ]),
        eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "pig2")],
             [val(0, "oink")
             ]),
        E7, E11
    ],
    ReParsed = [
        E1,
        eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "pig1")],
             [val(0, "grunt")
             ]),
        eqil([key(0, "foo"), key(2,"farm"), key(4, "pig"), key(6, "pig2")],
             [val(0, "oink")
             ]),
        eqil([key(0, "foo"), key(2,"farm"), key(4, "pig")],
             [val(6, "pig1 = grunt"),
              val(6, "pig2 = oink")
             ]),
        E7,
        E11,
        eqil([key(0, "foo"), key(2,"farm")],
             [val(4, "cow = moo=says hello"),
              val(4, "pig ="),
              val(6, "pig1 = grunt"),
              val(6, "pig2 = oink"),
              val(4, "chicken = cluck"),
              val(4, "info ="),
              val(6, "A chicken is an animal (chicken=animal) but not all"),
              val(6, "animals are chickens, so we cannot say = for animal"),
              val(6, "and chicken.")
             ]),
        eqil([key(0, "foo")],
             [val(2,"farm ="),
              val(4, "cow = moo=says hello"),
              val(4, "pig ="),
              val(6, "pig1 = grunt"),
              val(6, "pig2 = oink"),
              val(4, "chicken = cluck"),
              val(4, "info ="),
              val(6, "A chicken is an animal (chicken=animal) but not all"),
              val(6, "animals are chickens, so we cannot say = for animal"),
              val(6, "and chicken.")
             ])
    ],
    check(Inp, Parsed, Out, Normalized, ReParsed, _Result).

% ----------------------------------------------------------------------

check(Inp, Parsed, EmittedResult) :-
    check(Inp, Parsed, Parsed, EmittedResult).

check(Inp, Parsed, ReParsed, EmittedResult) :-
    check(Inp, Parsed, Inp, ReParsed, EmittedResult).

check(Inp, Parsed, Out, ReParsed, EmittedResult) :-
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result, Parsed),

    writeln(not_normalizing_check), !,
    \+ normalize_eqil(Result, _),
    writeln(confirmed_not_normalizable), !,

    emit_eqil(Result, GenOut),
    writeln(emitted), !,
    writeln(GenOut),
    writeln(eeeee),
    (Out == GenOut, !
    ; compare_ignore_leading_trailing_blanks(GenOut, Out)
    ),
    writeln(parsing_emitted), !,
    parse_eng_eqil(emitted_out, Out, EmittedResult),
    writeln(parsed_emitted), !,
    writeln(EmittedResult),
    assertall(EmittedResult, ReParsed).

check(Inp, Parsed, Out, already_normalized, ReParsed, EmittedResult) :-
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result, Parsed),

    writeln(not_normalizing_check),
    \+ normalize_eqil(Result, _),
    writeln(confirmed_not_normalizable),

    emit_eqil(Result, GenOut),
    writeln(emitted),
    writeln(GenOut),
    writeln(eeeee),
    (Out == GenOut, !
    ; compare_ignore_leading_trailing_blanks(GenOut, Out)
    ),
    writeln(parsing_emitted),
    parse_eng_eqil(emitted_out, Out, EmittedResult),
    writeln(parsed_emitted),
    writeln(EmittedResult),
    assertall(EmittedResult, ReParsed).

check(Inp, Parsed, Out, Normalized, ReParsed, EmittedResult) :-
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result, Parsed),

    writeln(normalizing_check),
    normalize_eqil(Result, Norm),
    writeln(confirmed_normalizable),
    assertall(Norm, Normalized),

    emit_eqil(Norm, GenOut),
    writeln(emitted),
    writeln(GenOut),
    writeln(eeeee),
    (Out == GenOut, !
    ; compare_ignore_leading_trailing_blanks(GenOut, Out)
    ),
    writeln(parsing_emitted),
    parse_eng_eqil(emitted_out, Out, EmittedResult),
    writeln(parsed_emitted),
    writeln(EmittedResult),
    assertall(EmittedResult, ReParsed).

compare_ignore_leading_trailing_blanks(Inp1, Inp2) :-
    string_trim(Inp1, TI1),
    string_trim(Inp2, TI2),
    assertion(TI1 == TI2).

remove_blank_lines(Inp, Out) :-
    split_string(Inp, "\n", "", Lines),
    rmv_blanks(Lines, NonBlankLines),
    intercalate(NonBlankLines, "\n", Out).
rmv_blanks([], []).
rmv_blanks([""|LS], OLS) :- !, rmv_blanks(LS, OLS).
rmv_blanks([L|LS], [L|OLS]) :- rmv_blanks(LS, OLS).

assertall(Got, Exp) :- assert_each_(0, Got, Exp).
assert_each_(_, [], []).
assert_each_(N, [], [_|_]) :- assertion(N == "Extra lines expected").
assert_each_(N, [_|_], []) :- assertion(N == "Extra lines received").
assert_each_(N, [G|Got], [G|Exp]) :- !, succ(N, P), assert_each_(P, Got, Exp).
assert_each_(N, [G|Got], [E|Exp]) :-
    assertion(N == (G == E)),
    succ(N, P), assert_each_(P, Got, Exp).

revert_assert_eng :-
    retractall(eng:key(_)),
    retractall(eng:key(_,_)),
    retractall(eng:key(_,_,_)),
    retractall(eng:key(_,_,_,_)),
    retractall(eng:key(_,_,_,_,_)),
    retractall(eng:key(_,_,_,_,_,_)),
    retractall(eng:key(_,_,_,_,_,_,_)),
    retractall(eng:eng(_,_)),
    retractall(eng:eng(_,_,_)),
    retractall(eng:eng(_,_,_,_)),
    retractall(eng:eng(_,_,_,_,_)),
    retractall(eng:eng(_,_,_,_,_,_)),
    retractall(eng:eng(_,_,_,_,_,_,_)),
    retractall(eng:eng(_,_,_,_,_,_,_,_)).

:- end_tests(eqil).
