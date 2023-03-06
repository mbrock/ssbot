:- module(grok,
          [hear/1, past/2, grok/0, frob/2, frob/0, dull/1,
           cope/1,
           openapi_to_rdf/2,
           item/4,
           find/3,
           link/2,
           json/2,
           known_event/2,
           grok_all/1,
           relevant_fact/3,
           fact_line/4,
           search/2,
           fuse/1,
           save/1,
           telegram_update/3,
           item/3,
           erc20/3,
           sing/1,
           nuke/2,
           show/1,
           search_result/2,
           token_symbol/2,
           save_erc20_transfers/1,
           person_account/2,
           log_row/2,
           etherscan_person_address/2,
           deny_ethereum_resources/1,
           person_date_log/3,
           blabla/0
          ]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), []).
:- use_module(library(persistency)).
:- use_module(library(http/json)).
:- use_module(library(openapi), []).
:- use_module(library(lists), [member/2]).
:- use_module(qdrant, [embed/3]).
:- use_module(openai, [completion/3, ask/2, edit/3]).
:- use_module(base).
:- use_module(apis).
:- use_module(coin).

:- persistent known_event(data:any, time:float).
:- db_attach("events.db", []).

:- op(921, fy, >-).
>- X --> [X].

:- rdf_meta >-(t, ?, ?).

:- op(920, fy, *).
* _.

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


study(Query, Result) :-
    qdrant:search(Query, 10, R),
    Results = R.result,
    findall(Line,
            (member(Result, Results),
             Text = Result.payload.text,
             rdf(Topic, schema:text, Text^^xsd:string),
             rdf(Source, schema:hasPart, Topic),
             rdf(Source, schema:author, Author^^xsd:string),
             rdf(Source, rdfs:label, Label@en),
             format(string(Line), "~w (~w): ``~w''",
                    [Label, Author, Text])),
            Lines),
    atomic_list_concat(Lines, '\n\n', Context),
    format(string(Prompt),
           "~w~n~n(end of quotes)~n~nThe theme is ~w. We can reference the quotes. Let's see...",
           [Context, Query]),
    ask(Prompt, Result0),
    normalize_space(string(Result), Result0).

search(Query, Answers) :-
    qdrant:search(Query, 10, Results),
    debug(telegram, "Results: ~w", [Results]),
    findall(Answer,
            (   member(Result, Results.result),
                Text = Result.payload.text,
                answer(Text, Answer)
            ),
            Answers).

search_result(Query, Result) :-
    qdrant:search(Query, 30, Results),
    member(Result, Results.result).

answer(Text, Answer) :-
    book_answer(Text, Answer), !.

answer(Text, Answer) :-
    correspondence_answer(Text, Answer), !.

book_answer(Text, Answer) :-
    rdf(Topic, schema:text, Text^^xsd:string),
    rdf(Topic, schema:isPartOf, Source),
    rdf(Source, rdfs:label, Title@en),
    rdf(Source, schema:author, Author^^xsd:string),
    format(string(Attribution), "~w, ~w", [Author, Title]),
    format(string(Content), "\"~w\"~n~n—~w", [Text, Attribution]),
    rdf(Source, schema:image, Image^^xsd:anyURI),
    Answer = _{ type: article,
                id: Topic,
                title: Attribution,
                input_message_content: _{message_text: Content},
                description: Text,
                thumb_url: Image
              }.

correspondence_answer(Text, Answer) :-
        rdf(Topic, as:content, Text^^xsd:string),
        rdf(Topic, as:attributedTo, Source),
        Title = "correspondence",
        rdf(Source, schema:givenName, Author^^xsd:string),
        format(string(Attribution), "~w, ~w", [Author, Title]),
        format(string(Content), "\"~w\"~n~n—~w", [Text, Attribution]),
        format(string(ImageUrl), "https://i.pravatar.cc/300?u=~w", [Author]),
        Answer = _{ type: article,
                        id: Topic,
                        title: Attribution,
                        input_message_content: _{message_text: Content},
                        description: Text,
                        thumb_url: ImageUrl
                  }.

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

link(_X, URL) :-
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

item(Dict, Path, Value) :-
    is_dict(Dict),
    Value = Dict.get(Path).

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
    ;   debug(grok, "No type for ~w~n", [X])).

grok(recv(telegram, _), nt:platform, nt:'Telegram').

grok(recv(telegram, X), rdf:type, as:'Note') :-
    item(X, message/text, string, _).

grok(recv(telegram, X), rdf:type, as:'Note') :-
    item(X, channel_post/chat/title, string, _).

grok(recv(telegram, X), as:content, Text^^xsd:string) :-
    item(X, channel_post/text, string, Text).

%grok(recv(telegram, X), as:attributedTo, AuthorName^^xsd:string) :-
%    item(X, channel_post/author_signature, string, AuthorName).

grok(recv(telegram, X), as:audience, Channel) :-
    item(X, channel_post/chat/username, string, ChannelUsername),
    item(X, channel_post/chat/id, integer, ChannelID),
    item(X, channel_post/chat/title, string, ChannelTitle),
    find(Channel, nt:telegramID, ChannelID),
    know(Channel, rdf:type, as:'OrderedCollection'),
    know(Channel, rdfs:label, ChannelTitle^^xsd:string),
    know(Channel, nt:username, ChannelUsername^^xsd:string).

grok(recv(telegram, X), nt:telegramId, V) :-
    item(X, message/message_id, integer, V).

grok(recv(telegram, X), as:content, V) :-
    item(X, message/text, string, V).

grok(recv(telegram, X), nt:embeddingId, UUID) :-
    item(X, message/text, string, Text),
    qdrant:embed(Text, _{text: Text}, UUID).

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

grok(recv(telegram, X), nt:koko, nt:me) :-
    item(X, message/reply_to_message/from/username, string, "riga_ss_bot").

grok(recv(telegram, X), as:audience, nt:me) :-
    item(X, message/chat/type, string, "private").

grok(recv(telegram, X), as:audience, Group) :-
    item(X, message/chat/type, string, "group"),
    item(X, message/chat/id, integer, ChatID),
    item(X, message/chat, is_dict, ChatData),
    item(X, message/chat/title, string, Title),

    find(Group, nt:telegramId, ChatID),
    json(Group, ChatData),

    know(Group, nt:platform, nt:'Telegram'),
    know(Group, rdf:type, as:'Group'),
    know(Group, rdfs:label, Title^^xsd:string).

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
    search(Query, Answers),
    api_post(telegram, [answerInlineQuery],
             json(_{inline_query_id: ID, results: Answers}),
             _).

grok(recv(telegram, X), nt:command, "study"^^xsd:string) :-
    item(X, message/text, string, Text),
    string_concat("/study ", Theme, Text),
    once(study(Theme, Answer)),
    api_post(telegram, [sendMessage],
             json(_{chat_id: X.message.chat.id,
                    text: Answer}),
             _).

grok(recv(telegram, X), nt:command, "ask"^^xsd:string) :-
    item(X, message/text, string, Text),
    item(X, message/message_id, integer, MessageID),
    item(X, message/chat/id, integer, ChatID),
    string_concat("/ask ", Question, Text),
    % Use the question in a GPT-3 prompt that requests an answer.
    format(string(Prompt), "Q: ~w\nA:", [Question]),
    once(ask(Prompt, Answer)),
    % Send the answer as a reply to the original message.
    api_post(telegram, [sendMessage],
             json(_{chat_id: ChatID,
                    text: Answer,
                    reply_to_message_id: MessageID}),
             _).


% The command "/code <something>" will ask OpenAI Codex to generate
% code for <something>.  The result is sent as a reply to the
% original message.

grok(recv(telegram, X), nt:command, "code"^^xsd:string) :-
    false,
    item(X, message/text, string, Text),
    item(X, message/message_id, integer, MessageID),
    item(X, message/chat/id, integer, ChatID),
    string_concat("/code ", Prompt, Text),
    once(edit("example(foo).", Prompt, Answer)),
    % Send the answer as a reply to the original message.
    api_post(telegram, [sendMessage],
             json(_{chat_id: ChatID,
                    text: Answer.text,
                    reply_to_message_id: MessageID}),
             Response),
    find(Answer, nt:telegramId, Response.result.message_id),
    json(Answer, Response.result),
    know(Answer, as:inReplyTo, X.message.message_id),
    know(Answer, as:generator, nt:codex).

grok(recv(telegram, X), nt:command, "info"^^xsd:string) :-
    item(X, message/text, string, Text),
    string_concat("/info ", Topic, Text),
    % get user full name
    item(X, message/from/first_name, string, FirstName),
    item(X, message/from/last_name, string, LastName),
    % get message ID for responding
    item(X, message/message_id, integer, MessageID),
    format(string(Name), "~w ~w", [FirstName, LastName]),
    % find personal data
    rdf(Subject, vcard:fn, Name^^xsd:string),
    % and now we want to present the subject in a GPT-3 prompt context...
    sing(string(P), subject(Subject)),
    % ...and use the topic as part of a prompt to get a good answer:
    format(string(Prompt), "Known relevant facts: ~w~n~nUser query: ~w~n~nAnswer (without any RDF technical syntaxes, English language):", [P, Topic]),
    % ...and ask GPT-3 to generate a response
    openai:ask(Prompt, Answer),
    % and finally send the answer to the user
    api_post(telegram, [sendMessage],
             json(_{chat_id: X.message.chat.id,
                    text: Answer,
                    reply_to_message_id: MessageID}),
             _).

grok(recv(telegram, X), nt:command, "coach"^^xsd:string) :-
    item(X, message/text, string, Text),
    string_concat("/coach", _, Text),
    item(X, message/chat/id, integer, ChatID),
    find(Conversation, nt:telegramId, ChatID),
    know(Conversation, nt:situation, nt:coach),
    api_post(telegram, [sendMessage],
             json(_{chat_id: ChatID,
                    text: "hey, what's up?"}),
             _).

% Respond to "callback queries" which look like this:
%   recv(telegram,_44130{callback_query:_44142{chat_instance:4706171842570693067,data:tldr,from:_44194{first_name:Mikael,id:362441422,is_bot:false,is_premium:true,language_code:en,last_name:Brockman,username:mbrockman},id:1556674054783516387,inline_message_id:BAAAACr_AADOapoVgdYHvexRJWM},update_id:289124583})

grok(recv(telegram, X), rdf:type, nt:'Callback') :-
    debug(telegram, "Callback: ~p", [X]),
    item(X, callback_query/data, string, Data),
    item(X, callback_query/id, string, _ID),
    item(X, callback_query/message/chat/id, integer, ChatID),
    item(X, callback_query/message/text, string, Text),
    debug(telegram, "Text: ~p", [Text]),
    (   Data = "tldr"
    ->  tldr(Text, TLDR),
        format(string(Reply), "~w", [TLDR]),
        api_post(telegram, [sendMessage],
                 json(_{chat_id: ChatID,
                        text: Reply}),
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

% The thing is, I've realized that this grok thing ought to be
% a DCG rule that relates a JSON object with a list of triples,
% using the RDF store for looking up existing resources.

has(X, Path, Value) :-
    member(Path-Value, X).

% Set rdf_meta on the >- operator so that we can use it in
% the DCG rule below.

:- op(921, fy, -<).
-< [D, P, V] --> {item(D, P, V)}.

example(telegram_update, _{
  message: _{
    chat: _{
      all_members_are_administrators: true,
      id: -866045078,
      title: "Node.Town Bot",
      type: "group"
    },
    date: 1677722672,
    from: _{
      first_name: "Mikael",
      id: 362441422,
      is_bot: false,
      is_premium: true,
      language_code: "en",
      last_name: "Brockman",
      username: "mbrockman"
    },
    message_id: 6281,
    text: "gör avancerade saker med unification"
  },
  update_id: 289125495
}).

payload(X, URL) -->
    { json_string(X, JSON) },
    >- [URL, nt:payload, JSON^^nt:json].

telegram_update(X) -->
    payload(X, URL),
    -< [X, update_id, ID],
    >- [URL, nt:telegramId, ID],
    telegram_activity(X, URL).

telegram_activity(X, URL) -->
    payload(X, URL),
    -< [X, message, Message],
    >- [URL, rdf:type, as:'Create'],
    >- [URL, as:object, MessageURL],
    telegram_message(Message, MessageURL).

telegram_message(X, URL) -->
    payload(X, URL),
    -< [X, message_id, ID],
    -< [X, date, UnixTime],
    -< [X, chat, Chat],
    >- [URL, nt:telegramId, ID],
    >- [URL, rdf:type, as:'Note'],
    >- [URL, as:published, UnixTime],
    >- [URL, as:audience, ChatURL],
    telegram_chat(Chat, ChatURL),
    telegram_message_content(X, URL),
    telegram_message_author(X, URL, ChatURL),
    telegram_message_parent(X, URL).

telegram_chat(X, URL) -->
    payload(X, URL),
    -< [X, id, ID],
    >- [URL, nt:telegramId, ID],
    telegram_chat_type(X, URL).

telegram_message_content(X, URL) -->
    -< [X, text, Text],
    >- [URL, as:content, Text].

telegram_chat_type(X, URL) -->
    -< [X, type, "private"],
    -< [X, first_name, FirstName],
    -< [X, last_name, LastName],
    { format(string(Name), "~w ~w", [FirstName, LastName]) },
    >- [URL, rdf:type, as:'Person'],
    >- [URL, as:name, Name].

telegram_chat_type(X, URL) -->
    -< [X, type, "group"],
    -< [X, title, Title],
    >- [URL, rdf:type, as:'Group'],
    >- [URL, as:name, Title].

telegram_message_author(X, URL, ChatURL) -->
    -< [X, from, From],
    >- [AuthorURL, as:partOf, ChatURL],
    >- [URL, as:attributedTo, AuthorURL],
    telegram_user(From, AuthorURL).

telegram_message_parent(X, URL) -->
    -< [X, reply_to_message, ReplyTo],
    !,
    >- [URL, as:inReplyTo, ReplyToURL],
    telegram_message(ReplyTo, ReplyToURL).

telegram_message_parent(_X, _URL) -->
    [].

telegram_user(X, URL) -->
    -< [X, id, ID],
    >- [URL, nt:telegramId, ID],
    telegram_user_type(X, URL),
    telegram_user_name(X, URL).

telegram_user_type(X, URL) -->
    -< [X, is_bot, true],
    >- [URL, rdf:type, as:'Service'].

telegram_user_type(X, URL) -->
    -< [X, is_bot, false],
    >- [URL, rdf:type, as:'Person'].

telegram_user_name(X, URL) -->
    -< [X, first_name, FirstName],
    -< [X, last_name, LastName],
    !,
    { format(string(Name), "~w ~w", [FirstName, LastName]) },
    >- [URL, as:name, Name].

telegram_user_name(X, URL) -->
    -< [X, first_name, FirstName],
    >- [URL, as:name, FirstName],
    >- [URL, schema:givenName, FirstName].

% Etherscan ERC20 transactions

example_tx(_{ blockHash: "0xffe0df860e1a17f25d6e21c646bbcb70459ee1a2d2360f26dcd9907416044b1b",
              blockNumber:"5554014",
              confirmations:"11186137",
              contractAddress:"0xd3ace836e47f7cf4948dffd8ca2937494c52580c",
              cumulativeGasUsed:"6240803",
              from:"0x0000000000000000000000000000000000000000",
              gas:"4000000",
              gasPrice:"6000000000",
              gasUsed:"3406248",
              hash:"0x22bdf92963263b452ccf1e2ba939b440241914c2c719b2fc7bef419f86714088",
              input: "deprecated",
              nonce:"368",
              timeStamp:"1525424510",
              to:"0x12f3a4f6deaebf4d8c78f66875a7af8d923c6752",
              tokenDecimal:"18",
              tokenName:"Free BOB Tokens - BobsRepair.com",
              tokenSymbol:"BOBx",
              transactionIndex:"83",
              value:"1500000000000000000000"}).

weird_token_symbol_name("0xecf8f87f810ecf450940c9f60066b4a7a501d6a7", "WETH", "Wrapped Ether").
% weird_token_symbol_name("0x6b175474e89094c44da98b954eedeac495271d0f", "DAI", "Dai Stablecoin").
% weird_token_symbol_name("0x89d24a6b4ccb1b6faa2625fe562bdd9a23260359", "SAI", "Sai Stablecoin").

token_symbol_name(X, Symbol, Name) :-
    X.tokenSymbol = "",
    !,
    Symbol = "WETH",
    Name = "Wrapped Ether".

token_symbol_name(X, Symbol, Name) :-
    X.tokenSymbol = Symbol,
    X.tokenName = Name.
              
erc20(X) -->
    -< [X, hash, Hash],
    -< [X, timeStamp, UnixTimeString],
    -< [X, from, SrcAddress],
    -< [X, to, DstAddress],
    -< [X, value, ValueString],
    -< [X, tokenDecimal, DecimalString],
    -< [X, contractAddress, ContractAddress],

    { token_symbol_name(X, Symbol, Name) },

    { number_string(Decimals, DecimalString) },
    { number_string(Value, ValueString) },
    { number_string(UnixTime, UnixTimeString) },
    
    { format(string(EtherscanURL),
             "https://etherscan.io/tx/~w", [Hash]) },
    
    >- [Txn, eth:txHash, Hash^^xsd:string],
    >- [Log, (eth:txHash)-(erc20:from), Txn-Src],
    >- [Src, eth:address, SrcAddress^^xsd:string],
    >- [Dst, eth:address, DstAddress^^xsd:string],
    >- [Gem, erc20:symbol, Symbol^^xsd:string],

    >- [Src, rdf:type, eth:'Account'],
    >- [Dst, rdf:type, eth:'Account'],
    
    >- [Txn, rdf:type, eth:'Tx'],
    >- [Txn, eth:txTime, UnixTime^^xsd:dateTime],
    >- [Txn, eth:to, ContractAddress],
    >- [Txn, rdfs:seeAlso, EtherscanURL^^xsd:anyURI],
    >- [Txn, eth:hasLogEntry, Log],
    
    >- [Log, rdf:type, erc20:'TokenTransfer'],
    >- [Log, erc20:from, Src],
    >- [Log, erc20:to, Dst],
    >- [Log, erc20:token, Gem],
    >- [Log, erc20:value, Value^^xsd:integer],
    
    payload(X, Log),
    
    >- [Gem, rdf:type, erc20:'ERC20Token'],
    >- [Gem, erc20:decimals, Decimals^^xsd:integer],
    >- [Gem, erc20:name, Name^^xsd:string],
    >- [Gem, erc20:symbol, Symbol^^xsd:string],

    ok.

:- rdf_meta nuke(o, r).

nuke(Type, S) :-
    rdf(S, rdf:type, Type),
    sing(subject(S)),
    deny(S, _, _),
    deny(_, _, S).

:- rdf_meta coin_type(r).

coin_type(erc20:'ERC20Token').
coin_type(eth:'Account').
coin_type(eth:'Tx').
coin_type(erc20:'TokenTransfer').

% OK, now we'll get these lists of triples containing free variables.
% Subjects and objects can be free variables.
% We'll use the RDF store to look up existing resources.
% If we find one, we'll use it.
% If we don't find one, we'll create a new one.

fuse(triples(Triples)) :-
    maplist(fuse(1), Triples),
    maplist(fuse(2), Triples).

fuse(1, [S, P1-P2, O1-O2]) :-
    var(S),
    ground(P1-P2),
    ground(O1-O2),
    !,
    rdf(S, P1, O1),
    rdf(S, P2, O2),
    !.

fuse(1, [S, P, O]) :-
    var(S),
    atom(P),
    ground(O),
    rdf(P, nt:idScope, _),
    rdf(S, P, O),
    !.

fuse(2, [S, P, O]) :-
    var(S),
    ground(P),
    ground(O),
    mint(url(S)),
    !.

fuse(_, [_S, _P, _O]).

% :- debug(grak).

save(triples(Triples)) :-
    maplist(save, Triples).

save([S, P, O]) :-
    know(S, P, O).

save(erc20(X)) :-
    phrase(erc20(X), Triples),
    fuse(triples(Triples)),
    rdf_transaction(save(triples(Triples)), save(erc20(X))).

:- rdf_meta save(t).
:- rdf_meta fuse(t).

:- dynamic sung/1.

:- rdf_meta sing(t).
:- rdf_meta sing(@, t).

sing(transaction(end(0), _)) :-
    findall(Quad, sung(Quad), Quads),

    get_time(TimeStamp),
    format_time(string(Time), '%F %T %Z', TimeStamp),
    
    ansi_format([fg(cyan)], '~w~n', [Time]),
    sing(quads(Quads)),
    retractall(sung(_)).

sing(assert(S, P, O, G)) :-
    debug(sing, 'Assert ~p', [S]),
    assertz(sung(rdf(S, P, O, G))).

sing(retract(S, P, O, G)) :-
    debug(sing, 'Retract ~w ~w ~w ~w', [S, P, O, G]).

sing(quads(Quads)) :-
    findall(S-(P-O),
            member(rdf(S, P, O, _), Quads),
            Changes),

    retractall(sung(_)),
    keysort(Changes, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    dict_create(Dict, rdf, Grouped),

    spew(Dict).

sing(subject(S)) :-
    findall(P-O,
            rdf(S, P, O, _),
            POs),
    keysort(POs, Sorted),
    dict_create(Dict, rdf, [S-Sorted]),
    nl,
    spew(Dict).

sing(string(String), X) :-
    with_output_to(
        string(String),
        sing(X)).

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
    openapi:openapi_read('api/telegram.yaml', Spec),
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

relevant_fact(Person, Relation, Object) :-
    rdf(Person, rdf:type, vcard:'Individual'),
    vcard_fact(Person, Relation, Object).

vcard_fact(Person, Relation, Object) :-
    rdf(Person, Relation, Object),
    vcard_relation(Relation).

:- rdf_meta rdf_text(o, -).

rdf_text(X^^xsd:string, X).
rdf_text(X@en, X).
rdf_text(X^^xsd:dateTime, X).
rdf_text(X^^xsd:integer, X).
rdf_text(X^^xsd:date, X).
rdf_text(X^^xsd:anyURI, X).
rdf_text(X, X) :- atom(X), \+ rdf_is_bnode(X).

fact_line(Person, Relation, Object, Line) :-
    relevant_fact(Person, Relation, Object),
    rdf(Relation, rdfs:label, Label),
    rdf_text(Label, LabelText),
    rdf_text(Object, ObjectText),
    format(string(Line), "~w: ~w", [LabelText, ObjectText]).

:- rdf_meta vcard_relation(r).

vcard_relation(vcard:fn).
vcard_relation(vcard:hasEmail).
vcard_relation(vcard:hasTelephone).
vcard_relation(vcard:hasAddress).
vcard_relation(vcard:'additional-name').
vcard_relation(vcard:'family-name').
vcard_relation(vcard:'given-name').
vcard_relation(vcard:bday).

:- debug(openapi).

today(DateTime) :-
    get_time(Now),
    stamp_date_time(Now, DateTime, local).

% Let's write a transaction report.
%
% For some token, and some account, we want a list of transactions
% with running balance.
%
% This is a DCG that relates a list of sorted transactions to a list
% of transactions with running balance.
%
% We use the "semicontext notation" for maintaining the balance
% state.
% We use contraints for integer arithmetic via library(clpfd).

:- use_module(library(clpfd)).

txn_sums([]), [_Sum] --> [].
txn_sums([Wad | Rest]), [Sum] -->
    { New #= Sum + Wad },
    [New],
    txn_sums(Rest).
        
% The CSV columns are:
%
% # Date, Symbol, Amount, Balance (one column per symbol), Debit, Credit
%
% We generate row sequences for each symbol, and then merge them
% together.


:- rdf_meta token_symbol(r, -),
            person_account(r, r),
            person_address(r, -),
            account_date_log(r, -, t),
            person_date_log(r, -, t),
            account_address(r, -).

account_address(Account, Address) :-
    rdf(Account, eth:address, Address^^xsd:string).
     
account_date_log(Account, Date, Log) :-
    (  rdf(Log, erc20:from, Account)
    ;  rdf(Log, erc20:to, Account)
    ),
    rdf(Txn, eth:hasLogEntry, Log),
    rdf(Txn, eth:txTime, Date^^xsd:dateTime).

person_account(Person, Account) :-
    rdf(Person, eth:controlsAccount, Account).

person_date_log(Person, Date, Log) :-
    person_account(Person, Account),
    account_date_log(Account, Date, Log).

token_symbol(Token, Symbol) :-
    rdf(Token, erc20:symbol, Symbol^^xsd:string).

save_erc20_transfers(Address) :-
    coin:etherscan(erc20_transfers(Address), Rows),
    forall(member(Row, Rows.result),
           save(erc20(Row))).

log_row(Log, log(Date, Symbol, Amount, Debit, Credit)) :-
    rdf(Txn, eth:hasLogEntry, Log),
    rdf(Txn, eth:txTime, Date^^xsd:dateTime),
    rdf(Log, erc20:token, Token),
    token_symbol(Token, Symbol),
    rdf(Log, erc20:from, Debit),
    rdf(Log, erc20:to, Credit),
    rdf(Log, erc20:value, Wad^^xsd:integer),
    rdf(Token, erc20:decimals, Decimals^^xsd:integer),
    Amount is Wad / 10^Decimals.

:- rdf_meta etherscan_person_address(r, ?).
etherscan_person_address(Person, Address) :-
    person_account(Person, Account),
    account_address(Account, Address),
    save_erc20_transfers(Address).

deny_ethereum_resources(T) :-
    coin_type(T),
    forall(nuke(T, _S), true).

% blabla(Gem) :-
%     rdf(Gem, eth:address, ),
%     know(Gem, erc20:symbol, "WETH"),
%     know(Gem, erc20:name, "Wrapped ETH").

