:- module(nt,
          [ graph/2,
            site/0,
            dial/0,
            static/2,
            recv_answers/2,
            stop/0,
            relevant/3,
            relevant/3,
            look/2,
            look/2,
            esbuild/0,
            sing/1,
            heading/4
          ]).

:- reexport(otp).
:- reexport(base).
:- reexport(grok).
:- reexport(hack).
:- reexport(openai).
:- reexport(discord).
:- reexport(telegram).
:- reexport(readwise).

:- use_module(qdrant, []).

:- debug(spin).

:- reexport(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [rdf_transaction/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/json), [json_read_dict/2]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).

:- use_module(library(pengines), []).

:- use_module(library(md/md_parse), [md_parse_string/2]).

:- http_handler(root(.), graph(transaction, html), []).
:- http_handler(root(graph), graph(transaction, ttl), []).

esbuild :-
  shell('cd web && npm run esbuild').

static(Path, File) :-
    http_handler(root(Path), http_reply_file(File, []), []).

:- static('index.js', 'web/dist/index.js').
:- static('tau-prolog.js', 'web/tau-prolog.js').

:- use_module(library(http/websocket)).

:- http_handler(root(websocket),
                websocket,
                [spawn([])]).

:- use_module(library(chan)).

:- use_module(library(http/http_session)).

:- dynamic socket/2.

:- http_set_session_options(
       [timeout(0),
        cookie(nodetown),
        proxy_enabled(true)]).

prep :-
    deny(nt:'login/telegram', as:content, _),
    know(nt:'login/telegram', as:content, "<script async src=https://telegram.org/js/telegram-widget.js?21 data-telegram-login=riga_ss_bot data-size=small data-radius=4 data-onauth='onTelegramAuth(user)' data-request-access=write></script>"^^nt:html).

websocket(Request) :-
    session(Context),
    http_upgrade_to_websocket(talk(Context), [], Request).

context(session(Session), Context) :-
    rdf(Context, nt:sessionId, Session^^xsd:string),
    !.

context(session(Session), Context) :-
    mint(url(Context)),
    know(Context, rdf:type, nt:'Context'),
    know(Context, nt:sessionId, Session^^xsd:string).

session(Context) :-
    http_in_session(Session),
    context(session(Session), Context).

graph(transaction, Format, Request) :-
    rdf_transaction(graph(Format, Request), graph(Format, Request)).

graph(html, Request) :-
    make,
    session(Context),
    know(Context, nt:action, nt:'login/telegram'),
    http_parameters(Request,
                    [query(Query, [default('')])]),
    reply_html_page(
        [ title('node.town'),
          meta([name(viewport),
                content('width=device-width, initial-scale=1.0')]),
          link([rel(stylesheet),
                href('https://font.node.town/index.css')]),
          link([rel(stylesheet),
                href('https://fonts.cdnfonts.com/css/univers-lt-pro')]),
%          script([src('tau-prolog.js')], []),
          script([src('index.js'), async], []),
          \style
        ],
        main([h1('node.town'),
              \graph_view(Context, Query),
              div([id(editor), autofocus], [])])).

graph(ttl, _Request) :-
    format('content-type: text/turtle~n~n'),
    graph_url(G),
    turtle(current_output, G).

html_string(HTML, String) :-
    phrase(HTML, Tokens),
    with_output_to(string(String),
                   print_html(Tokens)).

relevant(X, X, _) :-
    rdf_subject(X),
    \+ rdf_is_bnode(X),
    \+ rdf(X, nt:closed, _).

relevant(X, Y, 0) :-
    rdf(X, _, Y),
    rdf_subject(Y),
    \+ rdf_is_bnode(Y),
    \+ rdf(Y, nt:closed, _).

relevant(X, Y, Depth) :-
    Depth > 0,
    rdf(X, _, Z),
    rdf_is_subject(Z),
    relevant(Z, Y, Depth-1).

back(X, Y) :-
    rdf(Y, _, X),
    \+ rdf_is_bnode(Y).

description(Subject, Pairs) :-
    findall(P-O, rdf(Subject, P, O), Pairs0),
    keysort(Pairs0, Pairs).

:- rdf_meta look(r, r).

look(_, nt:mbrock).
%look(_, 'file:///home/mbrock/ssbot/vocabs/foaf-captsolo.rdf#Uldis_Bojars').
%look(_, X) :-
%    rdf(X, rdf:type, as:'Profile').

%look(_, X) :-
%    rdf(X, rdf:type, schema:'Book').

look(_, X) :-
    rdf(X, nt:username, "realnodetown"^^xsd:string).

%look(_, X) :-
%    rdf(C, nt:username, "realnodetown"^^xsd:string),
%    rdf(X, nt:context, C).

% look(_, X) :-
%     rdf(X, rdf:type, schema:'Quotation').

%look(Context, Topic) :-
%    relevant(Context, Topic, 1).
%

descriptions(Context, Descriptions) :-
    (   setof(Topic, look(Context, Topic), Topics)
    ->  true
    ;   Topics = []),
    findnsols(20, Topic-Pairs,
              (member(Topic, Topics),
               description(Topic, Pairs)),
              Descriptions0),
    keysort(Descriptions0, Descriptions1),
    reverse(Descriptions1, Descriptions).

graph_view(Context, '') -->
    { debug(web, 'Context: ~p', [Context]),
      descriptions(Context, Descriptions) },
    html(section(
             \description_tables(Descriptions))).

graph_view(Context, Query) -->
    { debug(web, 'Context: ~p, Query: ~p', [Context, Query]),
      qdrant:search(Query, 10, R),
      Results = R.result,
      findall(Topic-Pairs,
              (member(Result, Results),
               Text = Result.payload.text,
               rdf(Topic, schema:text, Text^^xsd:string),
               description(Topic, Pairs)),
              Descriptions)
    },
    html(section(
             \description_tables(Descriptions))).

talk(Context, WebSocket) :-
    rdf_transaction(call((mint(url(Id)),
                          know(Id, rdf:type, nt:'WebSocket'),
                          know(Id, as:context, Context),
                          know(Id, rdf:comment, "This is a Prolog channel for a browser client."@en))),
                    talk(Context)),
    assertz(socket(Id, WebSocket)),
    talk(recv, Id, WebSocket),
    retract(socket(Id, WebSocket)).

tell(Id, Term) :-
    socket(Id, WebSocket),
    with_output_to(
        string(String),
        write_term(Term, [quoted(true)])),
    mint(url(ReqId)),
    know(ReqId, rdf:type, nt:'WebSocketRequest'),
    know(ReqId, nt:socket, Id),
    know(ReqId, nt:term, String),
    ws_send(WebSocket,
            json(_{ type: query,
                    id: ReqId,
                    goal: String})),

    recv_answers(WebSocket, ReqId).

recv_answers(WebSocket, ReqId) :-
    ws_receive(WebSocket, Data, [format(json)]),
    (   _{ id: ReqId, done: true } :< Data.data
    ->  true
    ;   _{ id: ReqId, answer: Answer } :< Data.data
    ->  write_term(Answer, [quoted(true)]), nl,
        recv_answers(WebSocket, ReqId)
    ;   _{ id: ReqId, error: Error } :< Data.data
    ->  throw(error(Error))
    ).

talk(recv, Id, WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    (   Message.opcode == close
    ->  rdf_transaction(know(Id, nt:closed, true))
    ;   rdf_transaction(talk(msg, Id, Message)),
        talk(recv, Id, WebSocket)
    ).

talk(msg, Id, Message) :-
    mint(url(MsgId)),
    debug(web, 'Message: ~p', [Message]),
    know(MsgId, as:context, Id),
    get_time(TimeStamp),
    know(MsgId, as:time, TimeStamp),
    (   Message.opcode == text
    ->  handle_json(Id, MsgId, Message.data)
    ;   true
    ).

handle_json(Id, MsgId, ["query", Query]) :-
    debug(web, 'Query: ~p', [Query]),
    know(MsgId, as:content, Query),
    tell(Id, format("hello ~w~n", [Query])).

handle_json(Id, _MsgId, ["auth", "telegram", Data]) :-
    item(Data, id, number, TelegramUserId),
    find(User, nt:telegramId, TelegramUserId),
    item(Data, first_name, string, FirstName),
    item(Data, last_name, string, LastName),
    item(Data, photo_url, string, PhotoUrl),
    item(Data, username, string, Username),
    format(string(Url), "https://t.me/~w", [Username]),
    know(User, schema:givenName, FirstName),
    know(User, schema:familyName, LastName),
    know(User, schema:image, PhotoUrl^^xsd:anyURI),
    know(User, schema:url, Url^^xsd:anyURI),
    rdf(Context, nt:socket, Id),
    know(Context, nt:auth, User).

info(X) :-
    print_message(informational, X).

moan(X) :-
    print_message(error, X).

dote(Goal) :-
    info(dote(Goal, work)),
    catch(
        call(Goal),
        E,
        (moan(dote(Goal, fail(E))),
         Seconds = 3,
         info(dote(Goal, wait(Seconds))),
         sleep(Seconds),
         dote(Goal))),
    info(dote(Goal, done)).

dial :-
    spin(discord, dote(discord)),
    spin(telegram, dote(telegram)).

site :-
    graph_url(G),
    format("%% nt: using graph ~w~n", [G]),
    rdf_create_graph(G),
    rdf_default_graph(_, G),

    load,

    rdf_monitor(sing, []),

    (   site(4000)
    ->  true
    ;   site(4001)
    ->  true
    ).

:- use_module(library(ansi_term), [ansi_format/4]).

:- dynamic sung/1.

:- rdf_meta sing(t).

sing(transaction(end(0), _)) :-
    findall(Quad, sung(Quad), Quads),

    get_time(TimeStamp),
    format_time(string(Time), '%F %T %Z', TimeStamp),
    
    ansi_format(user_output, [fg(cyan)], '~w~n', [Time]),
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
    nl(user_output),
    spew(Dict).

site(Port) :-
    catch((http_stop_server(Port, []),
           format('% nt: stopped my server on port ~w~n', [Port])),
          error(existence_error(http_server, _), _),
          writeln('% no server running')),

    catch((http_server(http_dispatch, [port(Port)]),
           format('% nt: started server on port ~w~n', [Port])),
          error(socket_error(eaddrinuse, _), _),
          (format('% nt: tcp port conflict ~w~n', [Port]),
           fail)).

stop(Port) :-
    catch((http_stop_server(Port, []),
           format('% nt: stopped my server on port ~w~n', [Port])),
          error(existence_error(http_server, _), _),
          true).

stop :-
    stop(4000),
    stop(4001).

:- rdf_meta show(t, t, ?, ?).

properties([]) --> [].
properties([P-O|T]) -->
    html(tr([td(\show(P, nil)),
             td(\show(O, P))])),
    properties(T).

description_table(Subject, Pairs) -->
    { debug(web, 'Description: ~p ~p', [Subject, Pairs]),
      anchor(Subject, Anchor, Prefix:Local) },
    html(
        article(
            [\heading(Subject, Prefix:Local),
             table(
                [id(Anchor)],
                \properties(Pairs))])).

heading(Subject, _) -->
    { rdf_subject(Subject),
      rdf(Subject, rdfs:label, Label@en) },
    !,
    html(header(h2(u(Label)))).

heading('_', _Local) --> !, html([]).

heading(Prefix, Local) -->
    html(h2(\show(Prefix:Local, nil))).

description_tables([]) --> [].
description_tables([Subject-Pairs|T]) -->
    description_table(Subject, Pairs),
    description_tables(T).

:- table show//2.

show(X, P) -->
    { rdf(P, nt:secrecy, nt:secret) },
    !,
    html(details([class(secret)],
                 [summary("redacted"), \show(X, true)])).

show(X^^'http://www.w3.org/2001/XMLSchema#string', _) -->
    html(u(X)).

show(X^^'http://www.w3.org/2001/XMLSchema#anyURI', 'https://schema.org/image') -->
    html(img([src(X)])).

show(X^^'http://www.w3.org/2001/XMLSchema#anyURI', _) -->
    html(a([href(X)], X)).

show(true^^'http://www.w3.org/2001/XMLSchema#boolean', _) -->
    html(u('true')).

show(X^^'https://node.town/json', _) -->
    html(details([summary("JSON"), pre(X)])).

show(X^^'https://node.town/markdown', _) -->
    { md_parse_string(X, DOM), ! },
    html(DOM).

show(X^^'https://node.town/html', _) -->
    { format(atom(Atom), '~w', [X]) },
    [Atom].

show(X^^_, _) -->
    { number(X) },
    html(tt(X)).

show(date(Y,M,D)^^_, _) -->
    html(u("~w-~w-~w"-[Y,M,D])).

show(date_time(Y,M,D,H,Min,S,Offset)^^_, _) -->
    html(u("~w-~w-~w ~w:~w:~w ~w"-[Y,M,D,H,Min,S,Offset])).

show('_':Local, _) -->
    html(u([span(class(colon), ':'),
               span(class(local), Local)])).

show(Prefix:Local, _) -->
    { rdf_global_id(Prefix:Local, Atom) },
    !,
    show(Atom, nil).

show(X, _) -->
    { atom(X),
      \+ rdf_is_bnode(X),
      rdf_resource(X),
      anchor_href(X, Href, Prefix:Name) },
    html(a([href(Href), 'data-prefix'(Prefix)],
           [span(class(prefix), Prefix),
            span(class(colon), :),
            span(class(local), Name)])).

show(X, _) -->
    { atom(X),
      \+ rdf_is_bnode(X),
      \+ rdf_resource(X) },
    html(a([href(X)], X)).

% render bnodes inline recursively
show(X, _) -->
    { atom(X),
      rdf_is_bnode(X),
      description(X, Pairs) },
    description_table(X, Pairs).

show(X, _) -->
    { atom(X) },
    html(u(X)).

show(@(X, Lang), _) -->
    { string(X), atom(Lang) },
    html(u([lang(Lang)], X)).

show(X, Y) -->
    { debug(web, 'show(~w, ~w)~n', [X, Y]) },
    html(s('~w (~w)'-[X, Y])).

:- table anchor/3.

anchor(X, Id, Prefix:Local) :-
    \+ rdf_is_bnode(X),
    Id = X,
    rdf_global_id(Prefix:Local, X).

anchor(X, Id, '_':Local) :-
    rdf_is_bnode(X),
    atom_concat('_:', Local, X),
    format(atom(Id), "_:~w", [Local]).

anchor_href(X, Href, Prefix:Local) :-
    anchor(X, Id, Prefix:Local),
    atom_concat('#', Id, Href).

css_line(Selector, Property, Value, Line) :-
    css_value(Value, Value1),
    format(string(Line), "~w { ~w: ~w }~n",
           [Selector, Property, Value1]).

css_value(Value, Value) :-
    ( atom(Value); number(Value) ),
    !.

css_value(Value, Value1) :-
    is_list(Value),
    !,
    maplist(css_value, Value, Values),
    atomic_list_concat(Values, ', ', Value1).

css_value(Value, Value1) :-
    format(string(Value1), "~p", [Value]).

css_line(Line) :-
    css(Selector, Property, Value),
    css_line(Selector, Property, Value, Line).

css(body, 'font-family', ["univers lt pro", helvetica, 'sans-serif']).
css(body, 'font-size', '16px').
css(body, 'line-height', '20px').
css('*', 'font-size', '16px').
css('*', '-webkit-font-adjust', none).
css(body, 'margin', 0).
css('h1, h2', 'margin', 0).
css(':target', 'background-color', 'lightyellow').
css('[id]^="nt:"]', 'font-style', italic).
css('[lang]', 'opacity', 0.8).
css('a', 'text-decoration', none).
css('section', 'flex-direction', 'column').
css('article h2', 'margin', 0).
css('article h2', 'display', 'inline-flex').
css('article h2', 'padding', '0.25rem 0.5rem').
css('table', 'border', '1px solid #aaa').
css('table, article h2', 'border-left-width', '4px').
css('table', 'max-width', '100%').
css('td', 'padding', '0 .5rem').
css(main, display, grid).
css(main, 'grid-template-rows', 'auto 1fr auto').
css('body, main', position, absolute).
css('body, main', top, 0).
css('body, main', left, 0).
css('body, main', right, 0).
css('body, main', bottom, 0).

css(section, overflow, auto).
css(section, padding, '1em').

css(h1, 'border-bottom', '1px solid #aaa').
css(h1, background, '#fbfff2').
css(h1, padding, '0.25rem 0.5rem').

css('#editor', 'grid-row', 3).
css('#editor', 'grid-column', 1).
css('#editor', 'border-top', '1px solid #aaa').
css('#editor .cm-scroller', 'font-family', ["berkeley mono", monospace]).

css('h1, h2', 'font-weight', normal).
css('pre, tt', 'font-family', ["berkeley mono", monospace]).
css('section', 'display', flex).
css('section', 'gap', '1em').
css('section', 'flex-wrap', wrap).
css('section', 'flex-direction', row).
css('td > p:first-child', 'margin-top', 0).
css('td > p:last-child', 'margin-bottom', 0).
css('td:nth-child(1)', 'padding-right', '.5em').
css('td:nth-child(1)', 'text-align', right).
css('td:nth-child(1)', 'vertical-align', top).
css('td:nth-child(1)', 'white-space', nowrap).
css('td:nth-child(2)', 'vertical-align', top).
css('.prefix', 'opacity', 0.6).
css('.colon', 'opacity', 0.4).
css('.local', 'opacity', 0.8).

css('table[id^="_:"]', 'border-width', '1px 3px').
css('table[id^="_:"]', 'border-radius', '10px').
css('table[id^="_:"]', 'background-color', 'rgba(0,0,0,0.02)').

css('img', 'width', '5rem').
css('img', 'height', 'auto').
css('img, .secret', 'border', '1px solid #aaa').
css('img, .secret', 'border-radius', '0.5rem').
css('img, .secret', 'box-shadow', '0 0 0.5rem rgba(0,0,0,0.1)').

css('.secret', display, inline).
css('.secret', padding, '.25rem .5rem .1rem').
%css('.secret', 'font-size', '0.8em').
css('.secret', 'line-height', '0.8em').

css('u:before', content, "«").
css('u:after', content, "»").
css(u, 'text-decoration', none).

style -->
    { findall(Line, css_line(Line), Lines) },
    html(style(Lines)).
