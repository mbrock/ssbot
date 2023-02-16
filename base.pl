:- module(nt_base,
          [ known_event/2
          , save_event/1,
            know/3,
            deny/3,
            grok/0
          ]).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(nodetown, Dir)).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), []).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_portray), []).
:- use_module(library(semweb/turtle)).

:- rdf_attach_db(user_app_data("nodetown.rdf.db"), []).
:- rdf_attach_library(nodetown("vocabs/void.ttl")).

:- use_module(library(persistency)).

:- use_module(library(sweet)).

:- persistent
    known_event(data:any, time:float).

:- db_attach("events.db", []).

save_event(X) :-
    get_time(Now),
    assert_known_event(X, Now),
    debug(event, "~p", [X]).

know(S, P, O) :- know(S, P, O).
deny(S, P, O) :- deny(S, P, O).

know(S, P, O, G) :- rdf_assert(S, P, O, G).
deny(S, P, O, G) :- rdf_retractall(S, P, O, G).

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

grok(receive(websocket, discord, X)) :-
    is_dict(X),
    string(X.t),
    X.t = Type,
    grok(discord, Type, X).

grok(discord, "MESSAGE_CREATE", Message) :-
    mint(url(S)),
    writeln(S),
    writeln(Message),

    know(S, rdf:type, 'as:Note').

grok :-
    known_event(E, _T),
    grok(E).

filter(S, P, O) :-
    G = 'http://www.w3.org/ns/activitystreams',
    rdf(S, P, owl:'ObjectProperty', G).

export(X) :-
    mint(url(G)),
    !,
    cleanup(deny(_, _, _, G)),
    foreach(
        rdf(S, P, O, 'http://www.w3.org/ns/activitystreams'),
        know(S, P, O, G)
    ),
    tmp_file_stream(F, Stream, [extension("ttl")]),
    cleanup(read_file_to_string(F, X, [])),
    cleanup(close(Stream)),
    rdf_save_turtle(
        stream(Stream),
        [align_prefixes(true),
         comment(false),
         indent(2),
         graph(G),
         tab_distance(0)]).
