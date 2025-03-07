:- begin_tests(lando).
:- use_module("lando").
:- use_module(library(strings)).

test(simple_nameWord, [nondet]) :-
    string_chars("blah", Input),
    writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    writeln(Tokens),
    phrase(lando:nameWord([], N, _P), Tokens, []),
    assertion(N == "blah").

test(wild_chars_nameWord, [nondet]) :-
    string_chars("blah/blah#blah^blah", Input),
    writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    writeln(Tokens),
    phrase(lando:nameWord([], N, _P), Tokens, Remaining),
    writeln(nw),writeln(N),
    assertion(N == "blah/blah#blah^blah"),
    assertion(Remaining = []).

test(wild_chars_start_nameWord, [nondet]) :-
    string_chars("/]@#$><[blah/blah#blah^blah", Input),
    writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    writeln(Tokens),
    phrase(lando:nameWord([], N, _P), Tokens, Remaining),
    writeln(nw),writeln(N),
    assertion(N == "/]@#$><[blah/blah#blah^blah"),
    assertion(Remaining = []).

test(wild_chars_and_invalid_start_nameWord, [nondet]) :-
    string_chars("/]@(blah/blah#blah^blah", Input),
    writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    writeln(Tokens),
    phrase(lando:nameWord([], N, _P), Tokens, Remaining),
    writeln(nw),writeln(N),
    assertion(N == "/]@"),
    assertion(Remaining = [c('(',1,3),w(blah,1,4),c('/',1,8),
                           w('blah#blah^blah',1,9)]).

test(invalid_char_starts_nameWord, [nondet]) :-
    string_chars("(blah/blah#blah^blah", Input),
    writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    writeln(Tokens),
    \+ phrase(lando:nameWord([], _N, _P), Tokens, _Remaining).

test(invalid_char_ends_nameWord, [nondet]) :-
    string_chars("blah/blah, and blah", Input),
    write('input '), writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    write('tokens '),writeln(Tokens),
    phrase(lando:nameWord([], N, _P), Tokens, Remaining),
    writeln(nw),writeln(N),
    assertion(N == "blah/blah"),
    assertion(Remaining = [c(',',1,9),token_s,w(and,1,11),token_s,w(blah,1,15)]).

test(invalid_char_ends_excluded_name_nameWord, [nondet]) :-
    string_chars("blah, and blah", Input),
    write('input '), writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    write('tokens '),writeln(Tokens),
    \+ phrase(lando:nameWord(["blah"], _N, _P), Tokens, _Remaining).

%% --------------------------------------------------

test(simple_indexValueWord, [nondet]) :-
    string_chars("blah: foo", Input),
    writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    writeln(Tokens),
    phrase(lando:indexValueWord(N, _P), Tokens, Remainder),
    assertion(N == "blah"),
    assertion(Remainder == [c(':',1,4),token_s,w(foo,1,6)]).

test(pre_space_indexValueWord, [nondet]) :-
    string_chars("blah/blah#blah^blah :foo", Input),
    writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    writeln(Tokens),
    phrase(lando:indexValueWord(N, _P), Tokens, Remaining),
    writeln(nw),writeln(N),
    assertion(N == "blah/blah#blah^blah"),
    assertion(Remaining == [token_s,c(':',1,20),w(foo,1,21)]).

test(post_space_indexValueWord, [nondet]) :-
    string_chars("blah/blah#blah^blah: foo", Input),
    writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    writeln(Tokens),
    phrase(lando:indexValueWord(N, _P), Tokens, Remaining),
    writeln(nw),writeln(N),
    assertion(N == "blah/blah#blah^blah"),
    assertion(Remaining == [c(':',1,19),token_s,w(foo,1,21)]).

test(internal_colon_indexValueWord, [nondet]) :-
    string_chars("http://galois.com/blah/blah", Input),
    writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    writeln(Tokens),
    phrase(lando:indexValueWord(N, _P), Tokens, Remaining),
    writeln(nw),writeln(N),
    assertion(N == "http://galois.com/blah/blah"),
    assertion(Remaining = []).

test(internal_and_trailing_colon_indexValueWord, [nondet]) :-
    string_chars("http://galois.com/blah/blah: ok", Input),
    writeln(Input),
    phrase(lando:lexical_analysis("testinp", AllTokens), Input, []),
    append(Tokens, [eof], AllTokens),
    writeln(Tokens),
    phrase(lando:indexValueWord(N, _P), Tokens, Remaining),
    writeln(nw),writeln(N),
    assertion(N == "http://galois.com/blah/blah"),
    assertion(Remaining = [c(':',1,27),token_s,w(ok,1,29)]).

:- end_tests(lando).
