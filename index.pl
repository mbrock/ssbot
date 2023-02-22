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

    catch((http_stop_server(4000, []),
           writeln('% nt: stopped server')),
          error(existence_error(http_server, _), _),
          writeln('% no server running')),

    http_server(http_dispatch, [port(4000)]).

graph(html, _Request) :-
    reply_html_page(
        [ title('node.town'),
          link([rel(stylesheet),
                href('https://font.node.town/index.css')]),
          link([rel(stylesheet),
                href('https://fonts.cdnfonts.com/css/univers-lt-pro')]),
          \style
        ],
        [h1('node.town'),
         \graph_view]).

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

css(body, 'font-family', ["univers lt pro", helvetica, 'sans-serif']).
css(body, 'font-size', '16px').
css(body, 'line-height', '20px').
css(body, 'padding', '10px 20px').

css('h2, pre, tt', 'font-family', ["berkeley mono", monospace]).
css(article, border, '1px solid #aaa').
css(article, 'box-shadow', '-4px 0px 0 0 #aaa').
css(article, padding, '.5em').
css(article, 'max-width', '36em').
css(article, 'min-width', '20em').
css('article > h2', margin, 0).
css('article > h2', 'margin-bottom', '.5em').
css('h1, h2', 'font-size', inherit).
css('h1, h2', 'font-weight', normal).
css(section, display, flex).
css(section, 'flex-wrap', wrap).
css(section, gap, '1em').
css('[lang]', opacity, 0.8).
css(a, 'text-decoration', none).
%css(a, 'border-bottom', '1px solid #0005').
css('td:nth-child(1)', 'text-align', right).
css('td:nth-child(1)', 'vertical-align', top).
css('td:nth-child(1)', 'padding-right', '.5em').
css('td:nth-child(1)', 'white-space', nowrap).
css('td:nth-child(2)', 'vertical-align', top).
css('td > p:first-child', 'margin-top', 0).
css('td > p:last-child', 'margin-bottom', 0).

css('[data-prefix=nt]', 'font-style', italic).

style -->
    { findall(Line, css_line(Line), Lines) },
    html(style(Lines)).

description(Subject, Pairs) :-
    rdf_resource(Subject),
    (rdf(Subject, _, _, nt:graph) -> true),
    findall(P-O, rdf(Subject, P, O), Pairs0),
    keysort(Pairs0, Pairs).

descriptions(Descriptions) :-
    findall(Subject-Pairs,
            description(Subject, Pairs),
            Descriptions0),
    keysort(Descriptions0, Descriptions).

graph_view -->
        { descriptions(Descriptions) },
        html(section(\description_tables(Descriptions))).

:- rdf_meta show(t, ?, ?).

properties([]) --> [].
properties([P-O|T]) -->
        html(tr([td(\show(P)),
                 td(\show(O))])),
        properties(T).

description_tables([]) --> [].
description_tables([Subject-Pairs|T]) -->
    { anchor(Subject, Anchor) },
    html(
        article(
            [id(Anchor)],
            [h2("~w"-Anchor),
             table(\properties(Pairs))])),
    description_tables(T).
    
show(X^^'http://www.w3.org/2001/XMLSchema#string') -->
    !, html(span(X)).

show(X^^'http://www.w3.org/2001/XMLSchema#anyURI') -->
    !, html(a([href(X)], X)).

show(X^^'https://node.town/json') -->
    !, html(details([summary("JSON"), pre(X)])).

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

show(X) -->
    { atom(X),
      rdf_resource(X),
      rdf_global_id(Prefix:Local, X),
      % rdf(X, rdfs:label, Label),
      anchor_href(X, Href) },
    html(a([href(Href), 'data-prefix'(Prefix)],
           [Prefix, ":", Local])).

show(X) -->
    { atom(X), rdf_resource(X) },
    html(a(href(X), X)).

show(X) -->
    { atom(X) },
    html(span(X)).

show(@(X, Lang)) -->
    { string(X), atom(Lang) },
    html(span([lang(Lang)], X)).

anchor(X, Href) :-
    rdf_global_id(Prefix:Local, X),
    format(atom(Href), "~w:~w", [Prefix, Local]).

anchor_href(X, Href) :-
    anchor(X, Local),
    format(atom(Href), "#~w", [Local]).
