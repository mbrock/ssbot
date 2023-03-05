:- module(turtle_dcg, [turtle_clause/3]).

:- use_module(library(charsio), [char_type/2]).

%% TODO: literals
%% TODO: blank nodes
%% TODO: directives, prefixes, base, comments, etc.
%% TODO: terminals, unicode escapes, etc.

turtle_clause(G) -->
    turtle_subject(S),
    turtle_predicate_object_list(S, G),
    ".",
    space.

space --> " ", space.
space --> "\n", space.
space --> [].

turtle_subject(S)   --> turtle_iri(S).
turtle_object(O)    --> turtle_iri(O).

turtle_predicate(rdf:type) --> "a", space.
turtle_predicate(P) --> turtle_iri(P).

turtle_iri(Prefix:Local) -->
    turtle_prefix(Prefix),
    ":",
    turtle_local(Local),
    space.

%% Here are a bunch of details about the terminals from the spec,
%% which we don't implement yet.
%%
%% PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] |
%%                   [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] |
%%                   [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] |
%%                   [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] |
%%                   [#x10000-#xEFFFF]
%% PN_CHARS_U    ::= PN_CHARS_BASE | '_'
%% PN_CHARS      ::= PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] |
%%                   [#x203F-#x2040]
%% PN_PREFIX     ::= PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
%% PN_LOCAL      ::= (PN_CHARS_U | ':' | [0-9] | PLX)
%%                   ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
%% PLX           ::= PERCENT | PN_LOCAL_ESC
%% PERCENT       ::= '%' HEX HEX
%% HEX           ::= [0-9] | [A-F] | [a-f]
%% PN_LOCAL_ESC  ::= '\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" |
%%                        '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' |
%%                        '?' | '#' | '@' | '%')

turtle_prefix([]) --> [].
turtle_prefix([C|Cs]) -->
    [C],
    { char_type(C, alpha) },
    turtle_prefix(Cs).

turtle_local([]) --> [].
turtle_local([C|Cs]) -->
    [C],
    { char_type(C, alpha) },
    turtle_local(Cs).

turtle_predicate_object_list(S, G) -->
    turtle_predicate_object_list_head(S, G0),
    turtle_predicate_object_list_tail(S, G0, G).

turtle_predicate_object_list_head(S, G) -->
    turtle_predicate(P),
    turtle_object_list(S, P, G).

turtle_predicate_object_list_tail(_, G, G) -->
    [].

turtle_predicate_object_list_tail(S, G0, G) -->
    ";",
    space,
    turtle_predicate(P),
    turtle_object_list(S, P, G1),
    turtle_predicate_object_list_tail(S, G0+G1, G).

turtle_object_list(S, P, [S-P-O|Rest]) -->
    turtle_object(O),
    ",",
    space,
    turtle_object_list(S, P, Rest).

turtle_object_list(S, P, [S-P-O]) -->
    turtle_object(O).
