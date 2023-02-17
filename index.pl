:- module(nt,
          [ serve/0
          ]).

:- use_module(base).
:- use_module(apis).
:- use_module(discord).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/json)).

:- use_module(library(arouter)).

:- route_get(/, index(html)).
:- route_get('graph.ttl', index(ttl)).

serve :-
    http_server(route, [port(4000)]).

index(html) :-
    reply_html_page(
        [ title('node.town'),
          link([rel(stylesheet),
                href('https://font.node.town/index.css')]),
          style(['body { font-family: "helvetica", monospace; }',
                 "article { border: 1px solid #ccc; padding: .5em; }",
                 "article > h2 { margin: 0; margin-bottom: 0.5em; }",
                 "h1, h2 { font-size: inherit; }",
                 "article { width: 30em; }",
                 "section { display: flex; flex-wrap: wrap; gap: 1em }"
                ])
        ],
        [h1('node.town inspector'),
         \graph_view]).

index(ttl) :-
    format('content-type: text/turtle~n~n'),
    turtle(current_output, default).

description(Subject, Pairs) :-
    rdf_resource(Subject),
    (rdf(Subject, _, _, default) -> true),
    findall(P-O, rdf(Subject, P, O), Pairs).

descriptions(Descriptions) :-
    findall(Subject-Pairs,
            description(Subject, Pairs),
            Descriptions).

graph_view -->
        { descriptions(Descriptions) },
        html(section(\description_tables(Descriptions))).

:- rdf_meta show(t, ?, ?).

properties([]) --> [].
properties([P-O|T]) -->
        html(tr([td([style("text-align: right; vertical-align: top; padding-right: 0.5em")], \show(P)),
                 td([style("vertical-align: top")], \show(O))])),
        properties(T).

description_tables([]) --> [].
description_tables([Subject-Pairs|T]) -->
        html(article([h2("~w"-Subject), table(\properties(Pairs))])),
        description_tables(T).

show(X^^_Y) -->
    { string(X) },
    html(span(X)).

show(X^^_) -->
    { number(X) },
    html(span(X)).

show(date(Y,M,D)^^_) -->
    html(span("~w-~w-~w"-[Y,M,D])).

show(date_time(Y,M,D,H,Min,S,Offset)^^_) -->
    html(span("~w-~w-~w ~w:~w:~w ~w"-[Y,M,D,H,Min,S,Offset])).

show(X) -->
    { atom(X), rdf_resource(X), rdf(X, rdfs:label, Label) },
    html(a([style("font-family: helvetica"), href(X)], \show(Label))).

show(X) -->
    { atom(X), rdf_resource(X) },
    html(a(href(X), X)).

show(X) -->
    { atom(X) },
    html(span(X)).

show(@(X, Lang)) -->
    { string(X), atom(Lang) },
    html(span([lang(Lang)], X)).


