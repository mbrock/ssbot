:- module(nt_base,
          [ known_event/2
          , save_event/1,
            know/3,
            deny/3,
            grok/0,
            dump/1
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

:- rdf_meta spew(r, r, r).
spew(S, P, O) :-
    ansi_format([bold], "~w", [S]),
    ansi_format([faint], " :: ~w :: ", [P]),
    ansi_format([bold], "~w~n", [O]).

:- rdf_meta know(r, r, r).
know(S, P, O) :-
    spew(S, P, O),
    rdf_assert(S, P, O).

:- rdf_meta deny(r, r, r).
deny(S, P, O) :- deny(S, P, O).

:- rdf_meta know(r, r, r, r).
know(S, P, O, G) :-
    spew(S, P, O),
    rdf_assert(S, P, O, G).

:- rdf_meta deny(r, r, r, r).
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
    grok(discord(Type, X)).

grok(discord("MESSAGE_CREATE", Message)) :-
    mint(url(S)),
    writeln(S),
    writeln(Message),

    know(S, rdf:type, as:'Note').

grok(telegram(X)) :-
    Id = X.get(message/message_id),
    Text = X.get(message/text),
    Username = X.get(message/from/username),
    Timestamp = X.get(message/date),

    number(Id),
    string(Text),
    string(Username),
    number(Timestamp),
    stamp_date_time(Timestamp, _Date, local),

    writeln(X),
    
    once(mint(url(S))),
    
    know(S, rdf:type, as:'Note'),
    know(S, nt:'net/telegramId', Id),
    know(S, as:content, Text),
    know(S, as:published, Timestamp),
    know(S, as:attributedTo, Username).

grok :-
    known_event(E, _T),
    grok(E).

filter(S, P, _O) :-
    G = 'http://www.w3.org/ns/activitystreams',
    rdf(S, P, owl:'ObjectProperty', G).

turtle(Stream, Graph) :-
    rdf_save_turtle(
        stream(Stream),
        [align_prefixes(true),
         comment(false),
         indent(2),
         graph(G),
         tab_distance(0)]).

dump(default) :-
    turtle(user_output, default).

dump(as) :-
    mint(url(G)),
    !,
    cleanup(deny(_, _, _, G)),
    foreach(
        rdf(S, P, O, 'http://www.w3.org/ns/activitystreams'),
        know(S, P, O, G)
    ),
    tmp_file_stream(F, Stream, [extension("ttl")]),
    cleanup((read_file_to_string(F, X, []), writeln(X))),
    cleanup(close(Stream)),
    turtle(Stream, G).

