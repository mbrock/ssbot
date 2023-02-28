:- module(base,
          [ know/3,
            deny/3,
            dump/1,
            deny/4,
            dump/2,
            deny/0,
            turtle/2,
            sync/1,
            graph_url/1,
            mint/1,
            turn/2,
            open/1,
            load/0,
            spew/4,
            spew/1
          ]).

:- use_module(library(ansi_term), [ansi_format/4]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [(rdf_meta)/1, rdf_load/2]).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_portray), []).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/rdf_ntriples), []).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(sweet)).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(nodetown, Dir)).

open(Database) :-
    format(atom(Filename), "~w.rdf.db", [Database]),
    rdf_attach_db(
       user_app_data(Filename),
       [access(read_write)]),
    format('attached ~w~n', [Filename]).

:- open(nodetown).

% load(Atom, Type) :-
%     format(atom(Filename), "vocabs/~w.~w", [Atom, Type]),
%     rdf_load(Path, [graph(Atom)]).
%
% load(Atom) :- load(Atom, ttl).

load :-
    rdf_attach_library(nodetown(ontology)),
    rdf_load_library(activitystreams),
    rdf_load_library(rdf),
    rdf_load_library(rdfs),
    rdf_load_library(owl),
    rdf_load_library(dc),
    rdf_load_library(schema),
    rdf_load_library(foaf),
    rdf_load_library(vcard).

graph_url('https://node.town/graph').

sync(X) :-
    format(atom(S), "http://~w:4000/graph", [X]),
    rdf_load(S, [graph('https://node.town/graph')]),
    graph_url(G),
    rdf_default_graph(G).

:- rdf_meta
       spew(r, r, o),
       spew(r, r, o, +),
       spew(r, o),
       spew(r),
       know(r, r, o),
       know(r, r, o, +),
       deny(r, r, o),
       deny(r, r, o, +).

spew(X) :-
    rdf_global_id(P:L, X),
    !,
    ansi_format(user_output, [faint], "~w:", [P]),
    ansi_format(user_output, [bold], "~w ", [L]).

spew(X) :-
    atom(X),
    ansi_format(user_output, [fg(cyan)], " ~q ", [X]).

spew(X) :-
    is_list(X),
    foreach(member(Predicate-Object, X),
            spew(Predicate, Object)).

spew(X) :-
    is_dict(X),
    dict_pairs(X, _, Pairs),
    foreach(member(Subject-Attrs, Pairs),
            (ansi_format(user_output, [bold], "⦿ ", []),
             spew(Subject),
             nl(user_output),
             spew(Attrs))),
    nl(user_output).

spew(literal(type(T, X))) :-
    ansi_format(user_output, [fg(green)], "~w ", [X]),
    ansi_format(user_output, [fg(blue)], "[~w] ", [T]).

spew(P-O) :-
    spew(P, O).

spew(S, P, O) :-
    spew(S),
    spew(P),
    spew(O),
    format(user_output, " .~n", []).

spew(S, P, O, G) :-
    ansi_format(user_output, [fg(yellow)], "<~w> ", [G]),
    spew(S, P, O).

spew(P, O) :-
    format(user_output, "  • ", []),
    spew(P),
    spew(O),
    format(user_output, " ~n", []).

know(S, P, O) :-
    graph_url(G),
    know(S, P, O, G).

know(S, P, O, G) :-
    % spew(S, P, O, G),
    rdf_assert(S, P, O, G).

deny :-
    deny(_, _, _, default).

deny(S, P, O) :-
    deny(S, P, O, _).

deny(S, P, O, G) :-
    rdf_retractall(S, P, O, G).

:- rdf_meta turn(t, t).
turn(tired(S0, P0, O0), wired(S1, P1, O1)) :-
    rdf(S0, P0, O0, G),
    rdf_retractall(S0, P0, O0, G),
    rdf_assert(S1, P1, O1, G).

alphabet(zb32, `ybndrfg8ejkmcpqxot1uwisza345h769`).

index(List, Index, Element) :-
    nth1(Index, List, Element).

randseqs(K, N, Seq) :-
    repeat,
    randseq(K, N, Seq).

mint(url(X)) :-
    mint(atom(A)),
    format(atom(X), "https://id.node.town/~s", A).

mint(atom(A)) :-
    mint(key(T, X)),
    format(atom(A), "~s/~s", [T, X]).

mint(key(T, X)) :-
    get_time(UnixTime),
    stamp_date_time(UnixTime, DateTime, local),
    format_time(string(T), "%Y%m%d", DateTime),
    alphabet(zb32, Alphabet),
    !,
    randseqs(10, 32, Seq),
    maplist(index(Alphabet), Seq, Chars),
    prefix([First], Chars),
    char_type(First, prolog_atom_start),
    !,
    atom_codes(X, Chars).

turtle(Stream, Graph) :-
    rdf_save_turtle(
        stream(Stream),
        [align_prefixes(true),
         comment(false),
         indent(4),
         graph(Graph),
         tab_distance(0)]).

dump(String, Goal) :-
    mint(url(Subgraph)), !,
    cleanup(deny(_, _, _, Subgraph)),
    foreach(call(Goal, Subgraph), true),
    open_string(String, Stream),
    cleanup(close(Stream)),
    turtle(Stream, Subgraph).

dump(default) :-
    turtle(user_output, default).

