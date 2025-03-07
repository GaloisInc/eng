:- begin_tests(eqil).
:- use_module("eqil").
:- use_module(library(strings)).

% Note: many tests are nondet because the show_warnings has separate predicates
% to handle no-warnings v.s. some-warnings, which leaves a choicepoint even
% though only one will be used.

test(empty, [nondet]) :-
    parse_eng_eqil(empty, {|string||
|
|}, Result),
    writeln(Result),
    assertion(Result == []).

test(simple_key_val, [nondet]) :-
    Inp = {|string||
| key = value
|},
    parse_eng_eqil(basic1, Inp, Result),
    writeln(Result),
    assertall(Result,
                  [ eqil([key(0, "key")], [val(0, "value")])
                  ]
             ),
    \+ normalize_eqil(Result, _),
    % n.b. Do not want asserted eng:key and eng:eng predicates from this test to
    % affect other tests.  We cannot perform the assert_eqil and the subsequent
    % tests inside of a snapshot because assertion failures in a snapshot are not
    % propagated out (so all tests automatically succeed).  Instead, use
    % revert_assert_eng to remove all eng:key and eng:eng predicates asserted.
    revert_assert_eng,
    assert_eqil(Result),
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
    parse_eng_eqil(basic2, Inp, Result),
    writeln(Result),
    assertall(Result,
                  [ eqil([key(0, "key")], [val(0, "value")]),
                    eqil([key(0, "key2")], [val(0, "value2"), val(0, "")]),
                    eqil([key(0, "another key")], [val(0, "another value")])
                  ]
                 ),
    \+ normalize_eqil(Result, _),
    revert_assert_eng,
    assert_eqil(Result),
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
    parse_eng_eqil(basic3, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key")], [val(0, "value")]),
                eqil([key(0, "key2")], [val(0, "value2"), val(0, "")]),
                eqil([key(0, "another key")], [val(0, "another value")]),
                eqil([key(0, "just a key")], [])
              ]
             ),
    \+ normalize_eqil(Result, _),
    revert_assert_eng,
    assert_eqil(Result),
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
    parse_eng_eqil(basic3, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key")], [val(0, "value")]),
                eqil([key(0, "key2")], [val(0, "This is a multi-line value"),
                                        val(7, "that is on two lines.")
                                       ]),
                eqil([key(0, "another key")], [val(0, "another value")
                                              ])
              ]
             ),
    \+ normalize_eqil(Result, _),
    revert_assert_eng,
    assert_eqil(Result),
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
    another value
|
|},
    parse_eng_eqil(basic3, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key")], [val(0, "value")]),
                eqil([key(0, "key2")], [val(0, "This is a multi-line value"),
                                        val(7, "that is on two lines.")]),
                eqil([key(0, "another key")], [val(2, "another value")
                                              ])
              ]
             ),
    \+ normalize_eqil(Result, _),
    revert_assert_eng,
    assert_eqil(Result),
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
    parse_eng_eqil(basic3, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key")], [val(0, "value")]),
                eqil([key(0, "key2")], [val(0, "This is a multi-line value"),
                                        val(7, "that is on two lines."),
                                        val(0, "")]),
                eqil([key(0, "another key")], [val(0, "another value")])
              ]
             ),
    revert_assert_eng,
    assert_eqil(Result),
    \+ normalize_eqil(Result, _),
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
    parse_eng_eqil(basic3, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key")], [val(0, "value")]),
                eqil([key(0, "key2"), key(3, "This is a multi-line value")],
                     [val(7, "that is on three lines")]),
                eqil([key(0, "key2")], [val(3, "This is a multi-line value"),
                                        val(7, "that is on three lines"),
                                        val(2, "and this is the third.")]),
                eqil([key(0, "key2"), key(2, "and this is the third.")], []),
                eqil([key(0, "another key")], [val(0, "another value")])
              ]
             ),
    \+ normalize_eqil(Result, _),
    revert_assert_eng,
    assert_eqil(Result),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is a multi-line value\nthat is on three lines\nand this is the third.")),
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
    parse_eng_eqil(basic3, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(4, "key")], [val(0, "value"), val(0, "")]),
                eqil([key(0, "key2"), key(3, "This is a multi-line value")],
                     [val(7, "that is on five lines"),
                      val(5, "staggered leftwards")
                     ]),
                eqil([key(0, "key2")], [val(0, ""),
                                        val(3, "This is a multi-line value"),
                                        val(7, "that is on five lines"),
                                        val(5, "staggered leftwards"),
                                        val(2, "and this is the fourth"),
                                        val(12, "except the last."),
                                        val(0, "")]),
                eqil([key(0, "key2"), key(2, "and this is the fourth")],
                     [val(12, "except the last."), val(0, "")]),
                eqil([key(0, "another key")], [val(0, "another value")])
              ]
             ),
    \+ normalize_eqil(Result, _),
    revert_assert_eng,
    assert_eqil(Result),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is a multi-line value\nthat is on five lines\nstaggered leftwards\nand this is the fourth\nexcept the last.")),
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
    parse_eng_eqil(basic3, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key")], [val(0, "value"), val(0, "")]),
                eqil([key(0, "key2"), key(3, "This is")],
                     [val(0, "a multi-line value"),
                      val(7, "that is on three lines =")]),
                eqil([key(0, "key2"),
                      key(3, "This is"),
                      key(7, "that is on three lines")],
                     []),
                eqil([key(0, "key2")], [val(3, "This is = a multi-line value"),
                                        val(7, "that is on three lines ="),
                                        val(2, "= and = this is the third.")]),
                eqil([key(0, "key2"), key(2, "")],
                     [val(0, "and = this is the third.")]),
                eqil([key(0, "another key")], [val(0, "another value")])
              ]
             ),
    normalize_eqil(Result, Normalized),
    assertall(Normalized,
              [ eqil([key(0, "key")], [val(0, "value"), val(0, "")]),
                eqil([key(0, "key2"), key(3, "This is")],
                     [val(0, "a multi-line value"),
                      val(7, "that is on three lines =")]),
                eqil([key(0, "key2"),
                      key(3, "This is"),
                      key(7, "that is on three lines")],
                     []),
                %% eqil([key(0, "key2")], [val(3, "This is = a multi-line value"),
                %%                         val(7, "that is on three lines ="),
                %%                         val(2, "= and = this is the third.")]),
                eqil([key(0, "key2"),
                      key(2, "key21")],
                     [val(0, "and = this is the third.")]),
                eqil([key(0, "another key")], [val(0, "another value")])
              ]
             ),
    revert_assert_eng,
    assert_eqil(Result),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is = a multi-line value\nthat is on three lines =\n= and = this is the third.")),
    assertion(eng:key(key2, 'This is')),
    assertion(eng:eng(key2, 'This is', "a multi-line value\nthat is on three lines =")),
    assertion(eng:key(key2, 'This is', 'that is on three lines')),
    assertion(eng:key(key2, '')),
    assertion(eng:eng(key2, '', "and = this is the third.")),
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
    parse_eng_eqil(basic3, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key")], [val(0, "value"), val(0, "")]),
                eqil([key(0, "key2"), key(3, "This is")],
                     [val(0, "a multi-line value"),
                      val(7, "that is on three lines =")]),
                eqil([key(0, "key2"),
                      key(3, "This is"),
                      key(7, "that is on three lines")],
                     []),
                eqil([key(0, "key2")], [val(0, ""),
                                        val(3, "This is = a multi-line value"),
                                        val(7, "that is on three lines ="),
                                        val(2, "= and = this is the third."),
                                        val(4, "and the fourth"),
                                        val(0, "")]),
                eqil([key(0, "key2"), key(2, "")],
                     [val(0, "and = this is the third."),
                      val(4, "and the fourth"),
                      val(0, "")]),
                eqil([key(0, "another key")], [val(0, "another value")])
              ]
             ),
    normalize_eqil(Result, Normalized),
    assertall(Normalized,
              [ eqil([key(0, "key")], [val(0, "value"), val(0, "")]),
                eqil([key(0, "key2"), key(3, "This is")],
                     [val(0, "a multi-line value"),
                      val(7, "that is on three lines =")]),
                eqil([key(0, "key2"),
                      key(3, "This is"),
                      key(7, "that is on three lines")],
                     []),
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
              ]
             ),
    revert_assert_eng,
    assert_eqil(Result),
    assertion(eng:key(key)),
    assertion(eng:eng(key, "value")),
    assertion(eng:key(key2)),
    assertion(eng:eng(key2, "This is = a multi-line value\nthat is on three lines =\n= and = this is the third.\nand the fourth")),
    assertion(eng:key(key2, 'This is')),
    assertion(eng:eng(key2, 'This is', "a multi-line value\nthat is on three lines =")),
    assertion(eng:key(key2, 'This is', 'that is on three lines')),
    assertion(eng:key(key2, '')),
    assertion(eng:eng(key2, '', "and = this is the third.\nand the fourth")),
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
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key"), key(2, "subkey")], [val(0, "subval1")]),
                eqil([key(0, "key")],
                     [val(2, "subkey = subval1"),
                      val(2, "subkey2 = subval2")
                     ]),
                eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
                eqil([key(0, "another key"), key(2, "!")],
                     [val(0, "another value with a blank key")]),
                eqil([key(0, "another key")],
                     [val(2, "! = another value with a blank key")])
              ]
             ),
    \+ normalize_eqil(Result, _),
    revert_assert_eng,
    assert_eqil(Result),
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
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key"), key(2, "subkey")], [val(0, "subval1")]),
                eqil([key(0, "key")],
                     [val(2, "subkey = subval1"),
                      val(2, "subkey2 = subval2")
                     ]),
                eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
                eqil([key(0, "another key"), key(2, "!")],
                     [val(0, "another value with a blank key")]),
                eqil([key(0, "another key")],
                     [val(2, "! = another value with a blank key")])
              ]
             ),
    \+ normalize_eqil(Result, _),
    revert_assert_eng,
    assert_eqil(Result),
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
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key"), key(2, "subkey")], [val(0, "subval1")]),
                eqil([key(0, "key")],
                     [val(2, "subkey = subval1"),
                      val(2, "subkey2 = subval2")
                     ]),
                eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
                eqil([key(0, "another key"), key(2, "")],
                     [val(0, "another value with a blank key")]),
                eqil([key(0, "another key")],
                     [val(2, "= another value with a blank key")])
              ]
             ),
    revert_assert_eng,
    assert_eqil(Result),
    normalize_eqil(Result, Normalized),
    assertall(Normalized,
              [ eqil([key(0, "key"), key(2, "subkey")], [val(0, "subval1")]),
                eqil([key(0, "key")],
                     [val(2, "subkey = subval1"),
                      val(2, "subkey2 = subval2")
                     ]),
                eqil([key(0, "key"), key(2, "subkey2")], [val(0, "subval2")]),
                eqil([key(0, "another key"),
                      key(2, "another key1")],
                     [val(0, "another value with a blank key")])
                %% eqil([key(0, "another key")],
                %%      [val(2, "= another value with a blank key")])
              ]
             ),
    assertion(eng:key(key)),
    assertion(eng:key(key, subkey)),
    assertion(eng:eng(key, subkey, "subval1")),
    assertion(eng:key(key, subkey2)),
    assertion(eng:eng(key, subkey2, "subval2")),
    assertion(eng:key('another key')),
    assertion(eng:key('another key', '')),
    assertion(eng:eng('another key', '', "another value with a blank key")),
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
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    Exp = [ eqil([key(0, "key"), key(2, "subkey"), key(4, "<first_subkey>")],
                 [val(0, "#1!")]),
            eqil([key(0, "key"), key(2, "subkey"), key(4, "another key")],
                 [val(0, "double sub"), val(18, "multi-line value")]),
            eqil([key(0, "key"), key(2, "subkey"), key(4, "key with no value")],
                 [val(0, "")]),
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
            eqil([key(0, "key"), key(2, "subkey"), key(4, "and")],
                 [val(0, ""),
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
    assertall(Result, Exp),
    \+ normalize_eqil(Result, _),
    assertion(Result == Exp),
    revert_assert_eng,
    assert_eqil(Result),
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
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key")],
                     [val(2, "exec ="),
                      val(4, "command --debug=true --inline=yes inpfile")
                     ]),
                eqil([key(0, "key"), key(2, "exec")],
                     [val(4, "command --debug=true --inline=yes inpfile")]),
                eqil([key(0, "key"), key(2, "exec"), key(4, "command --debug")],
                     [val(0, "true --inline=yes inpfile")]),
                eqil([key(0, "another key"), key(2, "")],
                     [val(0, "another value with a blank key")]),
                eqil([key(0, "another key")],
                     [val(2, "= another value with a blank key")])
              ]
             ),
    normalize_eqil(Result, Normalized),
    assertall(Normalized,
              [ eqil([key(0, "key")],
                     [val(2, "exec ="),
                      val(4, "command --debug=true --inline=yes inpfile")
                     ]),
                eqil([key(0, "key"), key(2, "exec")],
                     [val(4, "command --debug=true --inline=yes inpfile")]),
                eqil([key(0, "key"), key(2, "exec"), key(4, "command --debug")],
                     [val(0, "true --inline=yes inpfile")]),
                eqil([key(0, "another key"), key(2, "another key1")],
                     [val(0, "another value with a blank key")])
                %% eqil([key(0, "another key")],
                %%      [val(2, "= another value with a blank key")])
              ]
             ),
    revert_assert_eng,
    assert_eqil(Result),
    assertion(eng:key(key)),
    assertion(eng:key(key, exec)),
    assertion(eng:eng(key, exec, "command --debug=true --inline=yes inpfile")),
    assertion(eng:key(key, exec, 'command --debug')),
    assertion(eng:eng(key, exec, 'command --debug', "true --inline=yes inpfile")),
    assertion(eng:key('another key')),
    assertion(eng:key('another key', '')),
    assertion(eng:eng('another key', '', "another value with a blank key")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key, 'another key' ]),
    findall((K1,K2), eng:key(K1,K2), K2S),
    assertion(K2S == [ (key,exec), ('another key','') ]),
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
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "help")],
                     [val(0, "build operation for op")]),
                eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "exec")],
                     [val(8, "command --debug=true --inline=yes inpfile")]),
                eqil([key(0, "key"), key(2,"op"), key(4, "build"),
                      key(6, "exec"), key(8, "command --debug")],
                     [val(0, "true --inline=yes inpfile")]),
                eqil([key(0, "key"), key(2,"op"), key(4, "build")],
                     [val(6, "help = build operation for op"),
                      val(6, "exec ="),
                      val(8, "command --debug=true --inline=yes inpfile"),
                      val(6, "#= This is=my comment style =#"),
                      val(0, "")
                     ]),
                eqil([key(0, "key"), key(2,"op"), key(4, "build"), key(6, "#")],
                     [val(0, "This is=my comment style =#"),
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
              ]
             ),
    \+ normalize_eqil(Result, _),
    revert_assert_eng,
    assert_eqil(Result),
    assertion(eng:key(key)),
    assertion(eng:key(key, op)),
    assertion(eng:key(key, op, build)),
    assertion(eng:key(key, op, build, help)),
    assertion(eng:key(key, op, build, exec)),
    assertion(eng:key(key, op, build, '#')),
    assertion(eng:eng(key, op, build, help, "build operation for op")),
    assertion(eng:eng(key, op, build, exec, "command --debug=true --inline=yes inpfile")),
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
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key"), key(2,"op")], [val(0, "build")]),
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
              ]
             ),
    normalize_eqil(Result, Normalized),
    assertall(Normalized,
              [ eqil([key(0, "key"), key(2,"op")], [val(0, "build"),
                                                    val(0, "test"),
                                                    val(0, "run")]),
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
              ]
             ),
    revert_assert_eng,
    assert_eqil(Result),
    assertion(eng:key(key)),
    assertion(eng:key(key, op)),
    assertion(eng:eng(key, op, "build")),
    assertion(eng:eng(key, op, "test")),
    assertion(eng:eng(key, op, "run")),
    assertion(eng:key(key, '')),
    assertion(eng:eng(key, '', "left")),
    assertion(eng:eng(key, '', "center\nof things")),
    assertion(eng:eng(key, '', "right")),
    findall(K, eng:key(K), KS),
    assertion(KS == [ key ]),
    findall((K1,K2), eng:key(K1,K2), K2S),
    assertion(K2S == [ (key,op), (key,op), (key,op), (key,''), (key,''), (key,'') ]),
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
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification"), key(6, "name")],
                     [val(0, "foo")]),
                eqil([key(0, "system"), key(2, "spec")],
                     [val(4, "This is my Specification ="),
                      val(6, "name = foo"),
                      val(6, "file = foo.spec")
                     ]),
                eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification")],
                     [val(6, "name = foo"),
                      val(6, "file = foo.spec")
                     ]),
                eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification"), key(6, "file")],
                     [val(0, "foo.spec")]),
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
              ]
             ),
    normalize_eqil(Result, Normalized),
    assertall(Normalized,
              [ eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification"), key(6, "name")],
                     [val(0, "foo")]),
                eqil([key(0, "system"), key(2, "spec")],
                     [val(4, "This is my Specification ="),
                      val(6, "name = foo"),
                      val(6, "file = foo.spec"),
                      val(4, "Another spec = here"),
                      val(6, "name = 'nuther"),
                      val(6, "'not = used")
                     ]),

                eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification")],
                     [val(6, "name = foo"),
                      val(6, "file = foo.spec")
                     ]),
                eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification"), key(6, "file")],
                     [val(0, "foo.spec")]),
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
              ]
             ),
    revert_assert_eng,
    assert_eqil(Result),
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
    assertion(K2S == [ (system,spec), (system,spec) ]),
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
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification"), key(6, "")],
                     [val(0, "foo")]),
                eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification"), key(6, "")],
                     [val(0, "foo.spec")]),
                eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification"), key(6, "")],
                     [val(0, "used")]),
                eqil([key(0, "system"), key(2, "spec")],
                     [val(4, "This is my Specification ="),
                      val(6, "= foo"),
                      val(6, "= foo.spec"),
                      val(6, "= used"),
                      val(6, "= at this time")
                     ]),
                eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification")],
                     [val(6, "= foo"),
                      val(6, "= foo.spec"),
                      val(6, "= used"),
                      val(6, "= at this time")
                     ]),
                eqil([key(0, "system"), key(2, "spec"),
                      key(4, "This is my Specification"), key(6, "")],
                     [val(0, "at this time")]),
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
              ]),
    normalize_eqil(Result, Normalized),
    assertall(Normalized,
              [ eqil([key(0, "system"), key(2, "spec"),
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
              ]).

test(adjacent_keys, [nondet]) :-
    Inp = {|string||
| key1
|   key2 = value2
| key1
|   key3 = value3
|},
    parse_eng_eqil(testinp, Inp, Result),
    writeln(Result),
    assertall(Result,
              [ eqil([key(0, "key1")],
                     [val(2, "key2 = value2")]),
                eqil([key(0, "key1"), key(2, "key2")],
                     [val(0, "value2")]),
                eqil([key(0, "key1"), key(2, "key3")],
                     [val(0, "value3")]),
                eqil([key(0, "key1")],
                     [val(2, "key3 = value3")])
              ]),
    normalize_eqil(Result, Normalized),
    assertall(Normalized,
              [ eqil([key(0, "key1")],
                     [val(2, "key2 = value2"),
                      val(2, "key3 = value3")]),
                eqil([key(0, "key1"), key(2, "key2")],
                     [val(0, "value2")]),
                eqil([key(0, "key1"), key(2, "key3")],
                     [val(0, "value3")])
              ]).


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
