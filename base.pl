:- module(nt_base,
          [ known_event/2
          , save_event/1,
            know/3,
            deny/3,
            grok/0,
            dump/1,
            deny/4,
            dump/2,
            deny/0,
            turtle/2,
            sync/1
          ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [(rdf_meta)/1]).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_portray), []).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(persistency)).
:- use_module(library(sweet)).
:- use_module(library(yall)).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(nodetown, Dir)).

:- rdf_attach_db(
       user_app_data("nodetown.rdf.db"), []).

:- rdf_attach_library(
       nodetown("vocabs/void.ttl")).

:- rdf_load_library(activitystreams).
:- rdf_load_library(internet).
:- rdf_load_library(rdf).
:- rdf_load_library(rdfs).
:- rdf_load_library(owl).
:- rdf_load_library(dc).

sync(X) :-
    rdf_load_library(X).

:- persistent
       known_event(data:any, time:float).

:- db_attach("events.db", []).

save_event(X) :-
    get_time(Now),
    assert_known_event(X, Now),
    debug(event, "~p", [X]).

:- rdf_meta
       spew(r, r, o),
       know(r, r, o),
       know(r, r, o, +),
       deny(r, r, o),
       deny(r, r, o, +),
       grok(+, r, o).

spew(S, P, O) :-
    ansi_format([bold], "~w", [S]),
    ansi_format([faint], " :: ~w :: ", [P]),
    ansi_format([bold], "~w~n", [O]).

know(S, P, O) :-
    rdf_default_graph(G),
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

grok(X) :-
    grok(X, rdf:type, _),
    mint(url(S)), !,
    writeln(S),
    foreach(grok(X, P, O),
            (format("~w :: ~w :: ~w~n", [S, P, O]),
             know(S, P, O))).

item(Dict, Path, Type, Value) :-
    Value = Dict.get(Path),
    call(Type, Value).

grok(recv(telegram, _), nt:platform, nt:'Telegram').

grok(recv(telegram, X), rdf:type, as:'Note') :-
    item(X, message/text, string, _).

grok(recv(telegram, X), nt:telegramId, V) :-
    item(X, message/message_id, integer, V).

grok(recv(telegram, X), as:content, V) :-
    item(X, message/text, string, V).

grok(recv(telegram, X), as:published, V) :-
    item(X, message/date, number, Timestamp),
    unix_date(Timestamp, V).

grok(recv(telegram, X), as:attributedTo, V) :-
    item(X, message/from/username, string, V).

grok(recv(discord, _), nt:platform, nt:'Discord').

grok(recv(discord, X), rdf:type, as:'Note') :-
    item(X, t, string, "MESSAGE_CREATE").

grok(recv(discord, X), nt:discordId, V) :-
    item(X, d/id, string, V).

grok(recv(discord, X), as:content, O) :-
    item(X, d/content, string, O).

grok(recv(discord, X), as:published, O) :-
    item(X, d/timestamp, string, O).

grok(recv(discord, X), as:attributedTo, O) :-
    item(X, d/author/username, string, O).

grok :-
    known_event(E, _T),
    grok(E).

unix_date(Unix, Date) :-
    stamp_date_time(Unix, DateTime, local),
    DateTime = date(Y, M, D, HH, MM, SS, Offset, _, _),
    Date = date_time(Y, M, D, HH, MM, SS, Offset).

turtle(Stream, Graph) :-
    rdf_global_id(Graph, GraphURL),
    rdf_save_turtle(
        stream(Stream),
        [align_prefixes(true),
         comment(false),
         indent(4),
         graph(GraphURL),
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

