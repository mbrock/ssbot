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
:- use_module(library(http/json), []).
:- use_module(library(http/http_dispatch)).

:- use_module(library(pengines), []).

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
          style(['body { font-family: "helvetica", monospace; }',
                 "article { border: 1px solid #ccc; padding: .5em; }",
                 "article > h2 { margin: 0; margin-bottom: 0.5em; }",
                 "h1, h2 { font-size: inherit; }",
                 "article { width: 30em; }",
                 "section { display: flex; flex-wrap: wrap; gap: 1em }"
                ])
        ],
        [h1('node.town'),
         \graph_view]).

graph(ttl, _Request) :-
    format('content-type: text/turtle~n~n'),
    graph_url(G),
    turtle(current_output, G).

description(Subject, Pairs) :-
    rdf_resource(Subject),
    (rdf(Subject, _, _, nt:graph) -> true),
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
        html(tr([td([style("text-align: right; vertical-align: top; padding-right: 0.5em; white-space: nowrap")], \show(P)),
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


