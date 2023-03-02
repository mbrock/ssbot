:- module(base,
          [ know/3,
            info/1,
            moan/1,
            deny/3,
            dump/1,
            deny/4,
            dump/2,
            deny/0,
            turtle/2,
            sync/1,
            graph_url/1,
            mint/1,
            turn/2,
            open/1,
            load/0,
            spew/4,
            spew/3,
            spew/1,
            json_string/2,
            ok/2,
            shew/3,
            show/1,
            moan/1,
            shew/4,
            triples_descriptions/2,
            op(920, fy, *),
            op(921, fy, >-)
          ]).

:- use_module(json_fix).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [(rdf_meta)/1, rdf_load/2]).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_portray), []).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdfa), []).
:- use_module(library(semweb/rdf_ntriples), []).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(sweet)).

:- op(920, fy, *).
* _.

:- op(921, fy, >-).
>- X --> [X].

:- rdf_meta >-(t, ?, ?).

info(X) :-
    print_message(informational, X).

moan(X) :-
    print_message(error, X).

json_string(JSON, String) :-
    ground(String),
    !,
    open_string(String, In),
    json_read_dict(In, JSON).

json_string(JSON, String) :-
    var(String),
    !,
    with_output_to(
        string(String),
        json_write_dict(current_output, JSON, [width(72)])).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(nodetown, Dir)).

open(Database) :-
    format(atom(Filename), "~w.rdf.db", [Database]),
    rdf_attach_db(
       user_app_data(Filename),
       [access(read_write)]),
    format('attached ~w~n', [Filename]).

:- open(nodetown).

% load(Atom, Type) :-
%     format(atom(Filename), "vocabs/~w.~w", [Atom, Type]),
%     rdf_load(Path, [graph(Atom)]).
%
% load(Atom) :- load(Atom, ttl).

load :-
    rdf_attach_library(nodetown(ontology)),
    rdf_load_library(activitystreams),
    rdf_load_library(rdf),
    rdf_load_library(rdfs),
    rdf_load_library(owl),
    rdf_load_library(dc),
    rdf_load_library(schema),
    rdf_load_library(foaf),
    rdf_load_library(vcard),
    rdf_load_library(eth),
    rdf_load_library(erc20),
    rdf_load_library(valueflows).

graph_url('https://node.town/graph').

sync(X) :-
    format(atom(S), "http://~w:4000/graph", [X]),
    rdf_load(S, [graph('https://node.town/graph')]),
    graph_url(G),
    rdf_default_graph(G).

:- rdf_meta
       spew(r, r, o),
       spew(r, r, o, +),
       spew(r, o),
       spew(r),
       know(r, r, o),
       know(r, r, o, +),
       deny(r, r, o),
       deny(r, r, o, +).

spew(X) :-
    rdf_global_id(P:L, X), !,
    ansi_format([faint], "~w:", [P]),
    ansi_format([bold], "~w~26|~t ", [L]).

spew(X) :-
    atom(X),
    ansi_format([fg(cyan)], "~q", [X]).

spew(X) :-
    is_list(X),
    foreach(member(Predicate-Object, X),
            spew(Predicate, Object)).

spew(X) :-
    is_dict(X),
    dict_pairs(X, _, Pairs),
    foreach(member(Subject-Attrs, Pairs),
            (ansi_format([bold], "⦿ ", []),
             spew(Subject),
             nl(user_output),
             spew(Attrs))),
    nl(user_output).

spew(literal(type(T, X))) :-
    ansi_format([fg(green)], "~w", [X]),
    * ansi_format([fg(blue)], " [~w] ", [T]).

spew(X^^T) :-
    ansi_format([fg(green)], "~w", [X]),
    * ansi_format([fg(blue)], " [~w] ", [T]).

spew(X@en) :-
    ansi_format([fg(yellow)], "~w", [X]),
    * ansi_format([fg(blue)], " [~w] ", [en]).

spew(P-O) :-
    spew(P, O).

spew(S, P, O) :-
    spew(S),
    spew(P),
    spew(O),
    format(" .~n", []).

spew(S, P, O, G) :-
    ansi_format([fg(yellow)], "<~w> ", [G]),
    spew(S, P, O).

spew(P, O) :-
    format("  • ", []),
    spew(P),
    spew(O),
    format(" ~n", []).

know(S, P, O) :-
    graph_url(G),
    know(S, P, O, G).

know(S, P, O, G) :-
    % spew(S, P, O, G),
    rdf_assert(S, P, O, G).

deny :-
    deny(_, _, _, default).

deny(S, P, O) :-
    deny(S, P, O, _).

deny(S, P, O, G) :-
    foreach(rdf(S, P, O, G),
            info(deny(S, P, O, G))),
    rdf_retractall(S, P, O, G).

ok --> [].    

:- multifile prolog:message//1.

prolog:message(shew(X)) -->
    shew(X).

prolog:message(shew(S, POs)) -->
    shew(S, POs).

shew(X) -->
    { atom(X),
      rdf_global_id(P:L, X) },
    !,
    >- ansi([fg('#999999')], '~w', [P]),
    >- ansi([fg(default)], ':', []),
    >- ansi([fg(default), bold], '~w~26|~t ', [L]).

shew(X) -->
    { atom(X) },
    !,
    >- ansi([fg(cyan)], '~w', [X]).

shew(X^^T) -->
    { atom(T),
      rdf_global_id(P:L, T) },
    >- ansi([fg(green)], '~w', [X]),
    % >- ansi([fg(blue)], ' [~w:~w] ', [P, L]),
    ok.

shew(X@Lang) -->
    >- ansi([fg(yellow)], '~w', [X]),
    >- ansi([fg(blue)], ' [~w] ', [Lang]).

shew([]) -->
    [].

shew([H|T]) -->
    shew(H),
    shew(T).

shew(P-O) -->
    >- ' • ',
    shew(P),
    >- ' => ',
    shew(O),
    >- nl.

shew(S, POs) -->
    shew(S),
    >- nl,
    shew(POs).

shew(descriptions, []) -->
    [].

shew(descriptions, [(S-POs)|T]) -->
    shew(S, POs),
    shew(descriptions, T),
    >- nl.

triples_descriptions(Triples, Descriptions) :-
    findall(S-(P-O), member([S, P, O], Triples), T1),
    keysort(T1, T2),
    group_pairs_by_key(T2, Descriptions).

:- rdf_meta subject_triples(r, -).
subject_triples(S, Triples) :-
    findall([S, P, O], rdf(S, P, O), Triples).

:- rdf_meta turn(t, t).
turn(tired(S0, P0, O0), wired(S1, P1, O1)) :-
    rdf(S0, P0, O0, G),
    rdf_retractall(S0, P0, O0, G),
    rdf_assert(S1, P1, O1, G).

alphabet(zb32, `ybndrfg8ejkmcpqxot1uwisza345h769`).

index(List, Index, Element) :-
    nth1(Index, List, Element).

randseqs(K, N, Seq) :-
    repeat,
    randseq(K, N, Seq).

mint(url(X)) :-
    mint(atom(A)),
    format(atom(X), "https://id.node.town/~s", A),
    * format(user_error, "mint ~w~n", [X]).

mint(atom(A)) :-
    mint(key(T, X)),
    format(atom(A), "~s/~s", [T, X]).

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

turtle(Stream, Graph) :-
    rdf_save_turtle(
        stream(Stream),
        [align_prefixes(true),
         comment(false),
         indent(4),
         graph(Graph),
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

:- rdf_meta show(r).
show(S) :-
    subject_triples(S, Triples),
    triples_descriptions(Triples, [S-POs]),
    info(shew(S, POs)).
