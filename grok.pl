:- module(grok,
          [hear/1, past/2, grok/0, frob/2, frob/0, dull/1,
           cope/1
          ]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), []).
:- use_module(library(persistency)).
:- use_module(library(http/json)).
:- use_module(openai, [completion/3]).
:- use_module(base, [mint/1, know/3]).
:- use_module(otp, [spin/2]).
:- persistent known_event(data:any, time:float).
:- db_attach("events.db", []).

self(nt:me).

past(X, T) :- known_event(X, T).

hear(X) :-
    get_time(Now),
    assert_known_event(X, Now),
    debug(event, "~p", [X]),
    ignore(grok(X)).

%!  frob is multi.
%
%   The frob predicates define migrations of the event database.

frob(receive(websocket, discord, X),
     recv(discord, X)).

frob :-
    forall(past(X, T),
           (   frob(X, Y)
           ->  retract_known_event(X, T),
               assert_known_event(Y, T)
           ;   true
           )).

%!  id_scope_relation(?Relation, ?Service) is nondet.
%
%   True if the relation indicates an ID with a scope,
%   e.g. a "Telegram ID" or a "Twitter ID".
%
%   We mint our own IDs for things, but we also want to
%   be able to link to external IDs.  This predicate
%   indicates which relations are used for this purpose.

id_scope_relation(Relation, Service) :-
    rdf(Relation, nt:idScope, Service).

scoped_id(X, ID, Graph, URL) :-
    id_scope_relation(Relation, _Service),
    grok(X, Relation, ID),
    rdf(URL, Relation, ID, Graph).

link(X, URL) :-
    (   scoped_id(X, ID, Graph, URL), !,
        format("Linking ~w (~w) [~w]~n", [URL, ID, Graph])
    ->  true
    ;   mint(url(URL))
    ,   format("Minting ~w~n", [URL])
    ).

find(S, P, O) :-
    (  rdf(S, P, O)
    -> true
    ;  mint(url(S)),
       know(S, P, O)
    ).

item(Dict, Path, Type, Value) :-
    is_dict(Dict),
    Value = Dict.get(Path),
    call(Type, Value).

json(X, Data) :-
    with_output_to(
        string(String),
        json_write_dict(current_output, Data, [width(80)])),
    know(X, nt:jsonPayload, String).

:- multifile dull/1.

dull(post(telegram, ["getUpdates"], _)).

:- rdf_meta grok(+, r, o).

grok(X) :-
    grok(X, rdf:type, _),
    link(X, S), !,
    foreach(grok(X, P, O),
            (know(S, P, O))),
    format("linked ~w to ~w~n~n", [X, S]).

grok(X) :-
    \+ grok(X, rdf:type, _),
    (   dull(X)
    ->  true
    ;   format("No type for ~w~n", [X])).

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
    item(X, message/from/id, integer, ID),
    item(X, message/from, is_dict, From),
    find(V, nt:telegramId, ID),
    json(V, From).

grok(recv(telegram, X), as:inReplyTo, V) :-
    item(X, message/reply_to_message/message_id, integer, V).

grok(recv(telegram, X), as:audience, nt:me) :-
    item(X, message/chat/type, string, "private").

grok(recv(telegram, X), as:audience, Group) :-
    item(X, message/chat/type, string, "group"),
    item(X, message/chat/id, integer, ChatID),
    item(X, message/chat, is_dict, ChatData),
    
    find(Group, nt:telegramId, ChatID),
    json(Group, ChatData),

    know(Group, nt:platform, nt:'Telegram'),
    know(Group, rdf:type, as:'Group').
    
grok(recv(_, X), nt:jsonPayload, V) :-
    with_output_to(
        string(V),
        json_write_dict(current_output, X, [width(80)])).
    
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
    E = recv(_, _),
    grok(E, rdf:type, _),
    grok(E).

unix_date(Unix, Date) :-
    stamp_date_time(Unix, DateTime, local),
    DateTime = date(Y, M, D, HH, MM, SS, Offset, _, _),
    Date = date_time(Y, M, D, HH, MM, SS, Offset).

cope(X) :-
    rdf(X, rdf:type, as:'Note'),
    rdf(X, as:audience, nt:me),
    find(Response, as:inReplyTo, X),
    know(Response, rdf:type, as:'Note'),
    know(Response, as:generator, nt:openai),
    gpt3(Response).

cope(X) :-
    rdf(X, rdf:type, as:'Note'),
    rdf(X, as:generator, nt:openai),
    \+ rdf(X, as:content, _).

gpt3(X) :-
    rdf(X, as:inReplyTo, InReplyTo),
    rdf(InReplyTo, as:content, Content^^xsd:string),
    completion(
        Content,
        _{max_tokens: 500, temperature: 0.8},
        Response),
    item(Response, text, string, Text),
    know(X, as:content, Text^^xsd:string),
    respond(X).

respond(X) :-
    rdf(X, as:content, Content^^xsd:string),
    rdf(X, as:inReplyTo, InReplyTo),
    rdf(InReplyTo, nt:telegramId, TelegramID^^xsd:integer),
    apis:api_post(
        telegram,
        ["sendMessage"],
        json(_{chat_id: TelegramID, text: Content}),
        Result),
    json(X, Result).
