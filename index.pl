:- module(nt,
          [ graph/2,
            site/0,
            dial/0
          ]).

:- reexport(otp).
:- reexport(base).
:- reexport(grok).
:- reexport(hack).
:- reexport(openai).
:- reexport(discord).
:- reexport(telegram).

:- debug(spin).

:- reexport(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/json), [json_read_dict/2]).
:- use_module(library(http/http_dispatch)).

:- use_module(library(pengines), []).

:- use_module(library(md/md_parse), [md_parse_string/2]).

:- http_handler(root(.), graph(html), []).
:- http_handler(root(graph), graph(ttl), []).

:- shell('cd web && npm run esbuild').

:- http_handler(root('index.js'),
                http_reply_file('web/dist/index.js', []), []).
:- http_handler(root('index.js.map'),
                http_reply_file('web/dist/index.js.map', []), []).

:- use_module(library(http/websocket)).

:- http_handler(root(websocket),
                http_upgrade_to_websocket(talk, []),
                [spawn([])]).

talk(WebSocket) :-
    mint(url(Id)),
    know(Id, rdf:type, nt:'WebSocket'),
    spin(talk(Id, recv), talk(recv, Id, WebSocket)),
    spin(talk(Id, send), talk(send, Id, WebSocket)),
    wipe(talk(Id, _)).

talk(recv, Id, WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    (   Message.opcode == close
    ->  know(Id, nt:closed, true)
    ;   mint(url(MsgId)),
        know(MsgId, as:context, Id),
        get_time(TimeStamp),
        know(MsgId, as:time, TimeStamp),
        (   Message.opcode == text
        ->  know(MsgId, as:content, Message.data)
        ;   true
        ),
        talk(recv, Id, WebSocket)
    ).

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

    (   site(4000)
    ->  true
    ;   site(4001)
    ->  true
    ).

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

graph(html, _Request) :-
    reply_html_page(
        [ title('node.town'),
          link([rel(stylesheet),
                href('https://font.node.town/index.css')]),
          link([rel(stylesheet),
                href('https://fonts.cdnfonts.com/css/univers-lt-pro')]),
          script([src('index.js'), async], []),
          \style
        ],
        main([h1('node.town'),
              \graph_view,
              div([id(editor), autofocus], [])])).

graph(ttl, _Request) :-
    format('content-type: text/turtle~n~n'),
    graph_url(G),
    turtle(current_output, G).

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

css('h1, h2', 'margin', 0).
css(':target', 'background-color', 'lightyellow').
css('[id]^="nt:"]', 'font-style', italic).
css('[lang]', 'opacity', 0.8).
css('a', 'text-decoration', none).
css('section', 'flex-direction', 'column').

css('article h2', 'margin', 0).
css('article h2', 'display', 'inline-flex').
css('article h2', 'border', '1px solid #aaa').
css('article h2', 'border-bottom', none).
css('article h2', 'padding', '0 0.5rem').
css('article h2', 'background-color', '#fbfff2').
css('table', 'border', '1px solid #aaa').
css('table, article h2', 'border-left-width', '4px').
css('table', 'max-width', '100%').
%css('table', 'min-width', '20em').
%css('table', 'padding', '.5em').
css('td', 'padding', '0 .5rem').
css('body', 'font-family', ["univers lt pro", helvetica, 'sans-serif']).
css('body', 'font-size', '16px').
css('body', 'line-height', '22px').
css('body', 'margin', 0).

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
css(h1, padding, '0 0.5rem').

css('#editor', 'grid-row', 3).
css('#editor', 'grid-column', 1).
css('#editor', 'border-top', '1px solid #aaa').
css('#editor .cm-scroller', 'font-family', ["berkeley mono", monospace]).

css('h1, h2', 'font-size', inherit).
css('h1, h2', 'font-weight', normal).
css('pre, tt', 'font-family', ["berkeley mono", monospace]).
css('section', 'display', flex).
css('section', 'gap', '1em').
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

style -->
    { findall(Line, css_line(Line), Lines) },
    html(style(Lines)).

% relevant(X) :- rdf(X, rdf:type, _, nt:graph).
relevant(X) :-
    rdf_resource(X),
    \+ rdf_is_bnode(X),
    rdf(X, rdf:type, _, 'http://www.w3.org/ns/activitystreams').

description(Subject, Pairs) :-
    findall(P-O, rdf(Subject, P, O), Pairs0),
    keysort(Pairs0, Pairs).

descriptions(Descriptions) :-
    findall(Subject-Pairs,
            (relevant(Subject),
             description(Subject, Pairs)),
            Descriptions0),
    keysort(Descriptions0, Descriptions).

graph_view -->
        { descriptions(Descriptions) },
        html(section(
                 \description_tables(Descriptions))).

:- rdf_meta show(t, ?, ?).

properties([]) --> [].
properties([P-O|T]) -->
    html(tr([td(\show(P)),
             td(\show(O))])),
    properties(T).

description_table(Subject, Pairs) -->
    { anchor(Subject, Anchor, Prefix:Local) },
    html(
        article(
            [\heading(Prefix, Local),
             table(
                [id(Anchor)],
                \properties(Pairs))])).

heading('_', _Local) --> !, html([]).

heading(Prefix, Local) -->
    html(h2(\show(Prefix:Local))).

description_tables([]) --> [].
description_tables([Subject-Pairs|T]) -->
    description_table(Subject, Pairs),
    description_tables(T).

show(X^^'http://www.w3.org/2001/XMLSchema#string') -->
    html(span(X)).

show(X^^'http://www.w3.org/2001/XMLSchema#anyURI') -->
    html(a([href(X)], X)).

show(X^^'https://node.town/json') -->
    html(details([summary("JSON"), pre(X)])).

show(X^^'https://node.town/markdown') -->
    { md_parse_string(X, DOM), ! },
    html(DOM).

show(X^^_) -->
    { number(X) },
    html(tt(X)).

show(date(Y,M,D)^^_) -->
    html(span("~w-~w-~w"-[Y,M,D])).

show(date_time(Y,M,D,H,Min,S,Offset)^^_) -->
    html(span("~w-~w-~w ~w:~w:~w ~w"-[Y,M,D,H,Min,S,Offset])).

show('_':Local) -->
    html(span([span(class(colon), ':'),
               span(class(local), Local)])).

show(Prefix:Local) -->
    { rdf_global_id(Prefix:Local, Atom) },
    show(Atom),
    !.

show(X) -->
    { atom(X),
      \+ rdf_is_bnode(X),
      rdf_resource(X),
      anchor_href(X, Href, Prefix:Name) },
    html(a([href(Href), 'data-prefix'(Prefix)],
           [span(class(prefix), Prefix),
            span(class(colon), :),
            span(class(local), Name)])).

% render bnodes inline recursively
show(X) -->
    { atom(X),
      rdf_is_bnode(X),
      description(X, Pairs) },
    description_table(X, Pairs).

show(X) -->
    { atom(X) },
    html(span(X)).

show(@(X, Lang)) -->
    { string(X), atom(Lang) },
    html(span([lang(Lang)], X)).

anchor(X, Id, Prefix:Local) :-
    \+ rdf_is_bnode(X),
    rdf_global_id(Prefix:Local, X),
    format(atom(Id), "~w:~w", [Prefix, Local]).

anchor(X, Id, '_':Local) :-
    rdf_is_bnode(X),
    atom_concat('_:', Local, X),
    format(atom(Id), "_:~w", [Local]).

anchor_href(X, Href, Prefix:Local) :-
    anchor(X, Id, Prefix:Local),
    format(atom(Href), "#~w", [Id]).
