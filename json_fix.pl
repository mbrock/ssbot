:- module(json_fix, []).

:- reexport(library(http/json)).
:- use_module(library(clpfd)).

hi_surrogate(C) :- C #>= 0xD800, C #< 0xDC00.
lo_surrogate(C) :- C #>= 0xDC00, C #< 0xE000.

surrogate([Hi, Lo], Codepoint) :-
    hi_surrogate(Hi),
    lo_surrogate(Lo),
    Codepoint #= (Hi - 0xD800) * 0x400 + (Lo - 0xDC00) + 0x10000.

get_XXXX(Stream, Codepoint) :-
    get_code(Stream, C1),
    get_code(Stream, C2),
    get_code(Stream, C3),
    get_code(Stream, C4),
    code_type(C1, xdigit(D1)),
    code_type(C2, xdigit(D2)),
    code_type(C3, xdigit(D3)),
    code_type(C4, xdigit(D4)),
    Codepoint #= D1 * 0x1000 + D2 * 0x100 + D3 * 0x10 + D4.

get_surrogate(Stream, Hi, Codepoint) :-
    get_code(Stream, 0'\\),
    get_code(Stream, 0'u),
    get_XXXX(Stream, Lo),
    surrogate([Hi, Lo], Codepoint).

json:escape(0'", _, 0'") :- !.
json:escape(0'\\, _, 0'\\) :- !.
json:escape(0'/, _, 0'/) :- !.
json:escape(0'b, _, 0'\b) :- !.
json:escape(0'f, _, 0'\f) :- !.
json:escape(0'n, _, 0'\n) :- !.
json:escape(0'r, _, 0'\r) :- !.
json:escape(0't, _, 0'\t) :- !.
json:escape(0'u, Stream, C) :- 
    !,
    get_XXXX(Stream, H),
    (   hi_surrogate(H) ->
        get_surrogate(Stream, H, C)
    ;   C = H
    ).

json_emoji("\"\\ud83d\\udc95\"", "💕").

% to print a number in hex, use format/3
% format(Stream, "~16r", [0x1F495]).
    
:- begin_tests(misc).

% Maybe the problem is with reading emojis from JSON.
%
% Key words: UTF-16, surrogate pairs, UTF-8, emojis.
%
% OK, I now understand that JSON allows the encoding of
% glyphs as UTF-16 surrogate pairs, but the JSON library
% might not be able to read those properly?

% test(emoji_json) :-
%     json_emoji(Json, Text),
%     open_string(Json, In),
%     json:json_read_dict(In, Term),
%     assertion(Term == Text).
    
:- end_tests(misc).

