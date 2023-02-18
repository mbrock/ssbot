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
            mint/1
          ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [(rdf_meta)/1, rdf_load/2]).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_portray), []).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(sweet)).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(nodetown, Dir)).

:- rdf_attach_db(
       user_app_data("nodetown.rdf.db"),
       [access(read_write)]).

:- rdf_attach_library(
       nodetown("vocabs/void.ttl")).

:- rdf_load_library(activitystreams).
:- rdf_load_library(internet).
:- rdf_load_library(rdf).
:- rdf_load_library(rdfs).
:- rdf_load_library(owl).
:- rdf_load_library(dc).

graph_url('https://node.town/graph').

sync(X) :-
    format(atom(S), "http://~w:4000/graph", [X]),
    rdf_load(S, [graph('https://node.town/graph')]).

:- rdf_meta
       spew(r, r, o),
       know(r, r, o),
       know(r, r, o, +),
       deny(r, r, o),
       deny(r, r, o, +).

spew(S, P, O) :-
    ansi_format([bold], "~w", [S]),
    ansi_format([faint], " :: ~w :: ", [P]),
    ansi_format([bold], "~w~n", [O]).

know(S, P, O) :-
    graph_url(G),
    know(S, P, O, G).

know(S, P, O, G) :-
    spew(S, P, O),
    rdf_assert(S, P, O, G).

deny :-
    deny(_, _, _, default).

deny(S, P, O) :-
    deny(S, P, O, _).

deny(S, P, O, G) :-
    rdf_retractall(S, P, O, G).

alphabet(zb32, `ybndrfg8ejkmcpqxot1uwisza345h769`).

index(List, Index, Element) :-
    nth1(Index, List, Element).

randseqs(K, N, Seq) :-
    repeat,
    randseq(K, N, Seq).

mint(url(X)) :-
    mint(atom(A)),
    format(atom(X), "https://node.town/~s", A).

mint(atom(A)) :-
    mint(key(T, X)),
    format(atom(A), "id/~s/~s", [T, X]).

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

