:- module(grok,
          [hear/1, past/2, grok/0, frob/2, frob/0, dull/1,
           cope/1,
           openapi_to_rdf/2,
           item/4,
           find/3,
           link/2,
           json/2,
           known_event/2,
           grok_all/1
          ]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), []).
:- use_module(library(persistency)).
:- use_module(library(http/json)).
:- use_module(library(openapi), [openapi_read/2]).
:- use_module(openai, [completion/3, ask/2]).
:- use_module(base, [mint/1, know/3]).
:- use_module(otp, [spin/2]).
:- use_module(apis).
:- persistent known_event(data:any, time:float).
:- db_attach("events.db", []).

self(nt:me).

past(X, T) :- known_event(X, T).

hear(X) :-
    get_time(Now),
    assert_known_event(X, Now),
    debug(event, "~p", [X]),
    ignore(rdf_transaction(grok(X), grok(X))).

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
    scoped_id(X, ID, Graph, URL),
    !,
    format("Linking ~w (~w) [~w]~n", [URL, ID, Graph]).

link(X, URL) :-
    mint(url(URL)),
    format("Minting ~w~n", [URL]).

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
    know(X, nt:jsonPayload, String^^nt:json).

:- multifile dull/1.

dull(post(telegram, ["getUpdates"], _)).

:- rdf_meta grok(+, r, o).

grok(X) :-
    grok(X, rdf:type, _),
    link(X, S), !,
    foreach(grok(X, P, O),
            (know(S, P, O))),
    format(user_output, "linked ~w to ~w~n~n", [X, S]).

grok(X) :-
    \+ grok(X, rdf:type, _),
    (   dull(X)
    ->  true
    ;   format(user_error, "No type for ~w~n", [X])).

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

% Respond to "inline queries" which look like this:
%
%   recv(telegram,_11762{inline_query:_11774{chat_type:group,from:_11820{first_name:Mikael,id:362441422,is_bot:false,is_premium:true,language_code:en,last_name:Brockman,username:mbrockman},id:1556674055259396328,offset:,query:foobar},update_id:289124535})
%
% We respond with a list of results from Qdrant (see qdrant.pl).

grok(recv(telegram, X), rdf:type, nt:'Query') :-
    debug(telegram, "Query: ~p", [X]),
    item(X, inline_query/query, string, Query),
    debug(telegram, "Query: ~p", [Query]),
    string_length(Query, Len),
    Len > 3,
    item(X, inline_query/id, string, ID),
    qdrant:search(Query, 20, Results),
    debug(telegram, "Results: ~w", [Results]),
    findall(Answer,
            (   member(Result, Results.result),
                Text = Result.payload.text,
                rdf(Topic, schema:text, Text^^xsd:string),
                rdf(Topic, schema:isPartOf, Source),
                rdf(Source, rdfs:label, Title@en),
                rdf(Source, schema:author, Author^^xsd:string),
                format(string(Attribution), "~w, ~w", [Author, Title]),
                format(string(Content), "\"~w\"~n~n—~w", [Text, Attribution]),
                rdf(Source, schema:image, Image^^xsd:anyURI),
                ReplyMarkup = _{inline_keyboard: [[_{text: "tldr",
                                                     callback_data: "tldr"}]]},
                Answer = _{ type: article,
                            id: Topic,
                            title: Attribution,
                            input_message_content: _{message_text: Content},
                            description: Text,
                            thumb_url: Image,
                            reply_markup: ReplyMarkup
                          }
            ),
            Answers),
    
    api_post(telegram, [answerInlineQuery],
             json(_{inline_query_id: ID, results: Answers}),
             _).

% Respond to "callback queries" which look like this:
%   recv(telegram,_44130{callback_query:_44142{chat_instance:4706171842570693067,data:tldr,from:_44194{first_name:Mikael,id:362441422,is_bot:false,is_premium:true,language_code:en,last_name:Brockman,username:mbrockman},id:1556674054783516387,inline_message_id:BAAAACr_AADOapoVgdYHvexRJWM},update_id:289124583})

grok(recv(telegram, X), rdf:type, nt:'Callback') :-
    debug(telegram, "Callback: ~p", [X]),
    item(X, callback_query/data, string, Data),
    item(X, callback_query/id, string, ID),
    item(X, callback_query/message/message_id, integer, MessageID),
    item(X, callback_query/message/chat/id, integer, ChatID),
    item(X, callback_query/message/text, string, Text),
    debug(telegram, "Text: ~p", [Text]),
    (   Data = "tldr"
    ->  tldr(Text, TLDR),
        format(string(Reply), "~w", [TLDR]),
        api_post(telegram, [sendMessage],
                 json(_{chat_id: ChatID,
                        text: Reply,
                        reply_to_message_id: MessageID}),
                 _)
    ;   true).

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

grok(readwise(X), rdf:type, schema:'CreativeWork') :-
    item(X, category, string, Category),
    \+ Category = "tweets".

grok(readwise(_), nt:platform, nt:readwise).

grok(readwise(X), nt:payload, JSON^^nt:json) :-
    % Weird unicode characters in the JSON cause errors
    % unless we set the stream property `representation_errors`
    % to 'unicode'.
    with_output_to(
        string(JSON),
        ( set_stream(current_output, representation_errors(unicode))
        , json_write_dict(current_output, X, [width(80)]))).

grok(readwise(X), nt:readwiseId, Id) :-
    item(X, user_book_id, integer, Id).

% {
%   "asin":"B0071M88DQ",
%   "author":"Esther Perel",
%   "book_tags": [],
%   "category":"books",
%   "cover_image_url":"https://images-na.ssl-images-amazon.com/images/I/41i7NrDNxQL._SL75_.jpg",
%   "document_note":null,
%   "highlights": [
%     {
%       "book_id":945822,
%       "color":"yellow",
%       "created_at":"2019-10-06T20:08:15.112Z",
%       "end_location":null,
%       "external_id":null,
%       "highlighted_at":"2019-07-02T04:56:00Z",
%       "id":28500137,
%       "is_discard":false,
%       "is_favorite":false,
%       "location":489,
%       "location_type":"location",
%       "note":"",
%       "readwise_url":"https://readwise.io/open/28500137",
%       "tags": [],
%       "text":"Love is at once an affirmation and a transcendence of who we are.",
%       "updated_at":"2019-10-06T20:08:15.112Z",
%       "url":null
%     }, ...
%   ],
%   "readable_title":"Mating in Captivity",
%   "readwise_url":"https://readwise.io/bookreview/945822",
%   "source":"kindle",
%   "source_url":null,
%   "title":"Mating in Captivity",
%   "unique_url":null,
%   "user_book_id":945822
% }

%% Readwise: category "books" => schema:Book
grok(readwise(X), rdf:type, schema:'Book') :-
    item(X, category, string, "books").

%% Readwise: .readable_title = rdfs:label (@en)
grok(readwise(X), rdfs:label, Label@en) :-
    item(X, readable_title, string, Label).

%% Readwise: .asin => schema:isbn
grok(readwise(X), schema:isbn, O) :-
    item(X, asin, string, O).

%% Readwise: .author => schema:author
grok(readwise(X), schema:author, O) :-
    item(X, author, string, O).

%% Readwise: .cover_image_url => schema:image
grok(readwise(X), schema:image, O^^xsd:anyURI) :-
    item(X, cover_image_url, string, O).

%% Readwise: .readwise_url => schema:url
grok(readwise(X), schema:url, O) :-
    item(X, readwise_url, string, O).

%% Readwise: .highlights => schema:Quotation
grok(readwise(X), schema:hasPart, O) :-
    grok(readwise(X), nt:readwiseId, SourceId),
    rdf(Source, nt:readwiseId, SourceId),
    item(X, highlights, is_list, Highlights),
    member(Highlight, Highlights),
    item(Highlight, id, integer, ID),
    find(O, nt:readwiseId, ID),
    know(O, schema:isPartOf, Source),
    json(O, Highlight),
    know(O, nt:platform, nt:readwise),
    know(O, rdf:type, schema:'Quotation'),
    % .highlighted_at => schema:dateCreated
    item(Highlight, highlighted_at, string, Date),
    know(O, schema:dateCreated, Date^^xsd:dateTime),
    % .text => schema:text
    item(Highlight, text, string, Text),
    know(O, schema:text, Text).

grok :-
    known_event(E, _T),
    E = recv(_, _),
    grok(E, rdf:type, _),
    grok(E).

grok_all(X) :-
    forall((known_event(X, _T),
            grok(X, rdf:type, _)),
           grok(X)).

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

:- rdf_meta openapi_to_rdf(+, r).

telegram_api :-
    openapi_read('api/telegram.yaml', Spec),
    openapi_to_rdf(Spec, nt:'api/telegram').

openapi_to_rdf(Spec, API) :-
    item(Spec, openapi, string, "3.0.0"),
    item(Spec, info/title, string, Title),
    item(Spec, info/version, string, Version),

    know(API, rdf:type, schema:'WebAPI'),
    know(API, schema:name, Title^^xsd:string),
    know(API, schema:version, Version^^xsd:string),

    % servers
    item(Spec, servers, is_list, Servers),
    forall(
        member(Server, Servers),
        openapi_server(Server, API)),

    % paths
    item(Spec, paths, is_dict, Paths),
    forall(
        item(Paths, Path, is_dict, PathData),
        openapi_path(Path, PathData, API)).

openapi_server(Server, API) :-
    item(Server, url, string, URL),
    know(API, schema:urlTemplate, URL^^xsd:string).

openapi_path(Path, PathData, API) :-
    item(PathData, post/externalDocs/url, string, DocURL),
    format(atom(ID), '~w~w/POST', [API, Path]),
    know(ID, rdf:type, schema:'EntryPoint'),
    know(ID, schema:documentation, DocURL^^xsd:anyURI),
    know(ID, schema:isPartOf, API),
    openapi_request_body(API, PathData, ID).

openapi_request_body(API, PathData, ID) :-
    debug(openapi, 'PathData: ~w', [PathData]),
    ( item(PathData, post/requestBody/content/'application/json'/schema, is_dict, Schema)
    ; item(PathData, post/requestBody/content/'multipart/form-data'/schema, is_dict, Schema)
    ),
    !,
    item(Schema, type, string, "object"),
    item(Schema, properties, is_dict, Properties),
    forall(
        get_dict(Key, Properties, Value),
        openapi_request_body_property(API, Key, Value, ID)).

openapi_request_body_property(API, Key, Value, ID) :-
    debug(openapi, 'Property: ~w ~w', [Key, Value]),
    item(Value, description, string, Description),
    format(atom(PropertyID), '~w/~w', [ID, Key]),
    know(PropertyID, rdf:type, http:'Parameter'),
    know(PropertyID, http:paramName, Key^^xsd:string),
    know(PropertyID, schema:isPartOf, ID),
    know(PropertyID, schema:isPartOf, API),
    know(PropertyID, schema:description, Description^^nt:markdown).


tldr(Text, TLDR) :-
        format(string(Prompt),
           "~w~n~ntl;dr:",
           [Text]),
    ask(Prompt, Result0),
    normalize_space(string(TLDR), Result0).



:- debug(openapi).
