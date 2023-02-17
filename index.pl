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

% demo(html, URL) :-
%     http_open(URL, Stream, []),
%     call_cleanup(
%         load_html(Stream, DOM, []),
%         close(Stream)),
%     html_text(DOM, [width(500)]).

serve :-
    http_server(http_dispatch, [port(4000)]).

:- http_handler(root(.), say_hi, []).

say_hi(_Request) :-
    reply_html_page(
        [ title('node.town'),
          link([rel(stylesheet),
                href('https://font.node.town/index.css')]),
          style('body { font-family: "Berkeley Mono", monospace; }')
        ],
        [h1('node.town inspector'),
         \graph_view]).

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
        html(ul(\description_tables(Descriptions))).

:- rdf_meta show(t, ?, ?).

properties([]) --> [].
properties([P-O|T]) -->
        html(tr([td([style("text-align: right; vertical-align: top; padding-right: 0.5em")], \show(P)),
                 td([style("vertical-align: top")], \show(O))])),
        properties(T).

description_tables([]) --> [].
description_tables([Subject-Pairs|T]) -->
        html(li([h2("~w"-Subject), table(\properties(Pairs))])),
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


