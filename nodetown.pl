% This file is part of the Node.Town system.
% It is licensed under the GNU AGPL v3.0.
% See the COPYING file for more information.

:- module(nodetown, []).


% * Imports

:- use_module(library(sweet)). % for the =use/1= directive

:- use aggregate -> foreach/2.
:- use lists     -> member/2.
:- use pairs     -> group_pairs_by_key/3.

:- use 'semweb/rdf11'.

:- use 'semweb/rdf_db' -> (rdf_meta)/1,
                          rdf_load/2,
                          rdf_register_prefix/2.

:- use 'semweb/rdf_persistency'.
:- use 'semweb/rdf_portray'.
:- use 'semweb/turtle'.
:- use 'semweb/rdfa'.
:- use 'semweb/rdf_ntriples'.
:- use 'semweb/rdf_http_plugin'.
:- use 'semweb/rdf_zlib_plugin'.
:- use 'http/http_ssl_plugin'.

:- rdf_meta
       >-(t, ?, ?),
       know(r, r, o),
       know(r, r, o, +),
       deny(r, r, o),
       deny(r, r, o, +).

:- op(920, fy, *).
:- op(921, fy, >-).
:- op(650, fy, ~).


% * Preliminaries

% Add the source directory to the search path, so that we can
% refer to file resources as =nodetown(Name)=.

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(nodetown, Dir)).

%!  info(Message) is det.
%!  oops(Message) is det.

info(X) :- print_message(informational, X).
oops(X) :- print_message(error, X).

%!  *(Goal) is det.
%
%   With this prefix operator, we can easily comment out a goal.

* _.

%!  >-(Term)// is det.
%
%   This prefix operator is syntactic sugar for singleton DCG entries.
%   We will use this heavily when defining JSON-to-RDF mappings.
%
%   @arg Term can contain RDF =Prefix:Name= terms, which are expanded.

>- Term --> [Term].


%!  ~(IRI) is det.
%
%   For clarity and efficiency, we use a concise functor to
%   distinguish IRI atoms in RDF data from literals, etc.

~ IRI :- ~(IRI).

% * RDF Knowledge Graph Basics

:- rdf_register_prefix(nt, 'https://node.town/').
:- rdf_register_prefix(id, 'https://id.node.town/').
:- rdf_register_prefix(as, 'http://www.w3.org/ns/activitystreams#').
:- rdf_register_prefix(schema, 'https://schema.org/').
:- rdf_register_prefix(vcard, 'http://www.w3.org/2006/vcard/ns#').
:- rdf_register_prefix(eth, 'http://ethon.consensys.net/').
:- rdf_register_prefix(erc20, 'http://erc20.consensys.net/ERC20/').

lore_path(user_app_data('nodetown.rdf.db')).
lore_url('https://node.town/graph').

load :-
    lore_path(Path),
    rdf_attach_db(Path, [access(read_write)]),
    info(load_lore(Path)).

know(S, P, O) :-
    lore_url(G),
    know(S, P, O, G).

know(S, P, O, G) :-
    rdf_assert(S, P, O, G).

deny :-
    deny(_, _, _, default).

deny(S, P, O) :-
    deny(S, P, O, _).

deny(S, P, O, G) :-
    foreach(rdf(S, P, O, G),
            info(deny(S, P, O, G))),
    rdf_retractall(S, P, O, G).


% * Formatting Facts

:- multifile prolog:message//1.

prolog:message(show(X)) -->
    show(X).

prolog:message(show(S, POs)) -->
    show(S, POs).

show(~(X)) -->
    { rdf_global_id(P:L, X) },
    >- ansi([fg('#999999')], '~w', [P]),
    >- ansi([fg(default)], ':', []),
    >- ansi([fg(default), bold], '~w~26|~t ', [L]).

show(~(X)) -->
    >- ansi([fg(cyan)], '~w', [X]).

show(X^^T) -->
    { atom(T),
      rdf_global_id(_Prefix:_Name, T) },
    >- ansi([fg(green)], '~w', [X]).

show(X@Lang) -->
    >- ansi([fg(yellow)], '~w', [X]),
    >- ansi([fg(blue)], ' [~w] ', [Lang]).

show([]) -->
    [].

show([H|T]) -->
    show(H),
    show(T).

show(P-O) -->
    >- ' • ',
    show(P),
    >- ' => ',
    show(O),
    >- nl.

show(S, POs) -->
    show(S),
    >- nl,
    show(POs).

show(descriptions, []) -->
    [].

show(descriptions, [(S-POs)|T]) -->
    show(S, POs),
    show(descriptions, T),
    >- nl.

triples_descriptions(Triples, Descriptions) :-
    findall(S-(P-O), member([S, P, O], Triples), T1),
    keysort(T1, T2),
    group_pairs_by_key(T2, Descriptions).

:- rdf_meta subject_triples(r, -).
subject_triples(S, Triples) :-
    findall([S, P, O], rdf(S, P, O), Triples).
