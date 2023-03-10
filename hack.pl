:- module(hack,
          [ install_relevant_pack/3
          , save_pack_data/0
          , pack_embedding/2
          , known_pack/1,
            relevant_pack/3,
            git/1,
            know_pack/1,
            know_all_packs/0
          ]).

:- use_module(know).

:- use_module(library(persistency)).
:- use_module(library(prolog_pack)).

:- persistent
    known_pack(pack:any).

:- db_attach("hack.db", []).

% Some GPT-3 magic with Prolog package data.

pack_line(pack(Name, _, Description, _, _), Line) :-
    atomic_list_concat([Name, Description], ": ", Line).

pack_embedding(Pack, Embedding) :-
    pack_line(Pack, Line),
    embedding(Line, Embedding).

save_pack_data :-
    prolog_pack:query_pack_server(search(""), true(Result), []),
    forall(member(X, Result), assert_known_pack(X)),
    findall(Line, (member(Pack, Result), pack_line(Pack, Line)), Lines),
    save_many_embeddings(Lines).

known_pack_line(Line, Pack) :-
    known_pack(Pack),
    pack_line(Pack, Line),
    !.

relevant_pack(Text, Pack, Similarity) :-
    similarity_search(Text, Line, Similarity),
    Similarity > 0.75,
    known_pack_line(Line, Pack).

:- debug(pack).

install_relevant_pack(Text, Name, Similarity) :-
    relevant_pack(Text, Pack, Similarity),
    Pack = pack(Name, _, _, _, _),
    pack_install(Name, [interactive(false)]).

know_pack(pack(Name, p, Description, Version, Sources)) :-
    format(atom(Url), 'https://node.town/pkg/swi-prolog/~w', [Name]),
    atom_string(Description, DescriptionString),
    atom_string(Version, VersionString),
    know(Url, rdf:type, nt:'Package'),
    know(Url, nt:system, nt:'swi-prolog'),
    know(Url, rdf:label, Name),
    know(Url, nt:version, VersionString),
    know(Url, rdf:description, DescriptionString@en),
    forall(member(Source, Sources),
           ( atom_string(Source, SourceString),
             know(Url, nt:source, SourceString^^xsd:anyURI) )).

know_all_packs :-
    prolog_pack:query_pack_server(search(""), true(Result), []),
    forall(member(Pack, Result), know_pack(Pack)).

git(X) :-
    atomic_list_concat(['git', X], ' ', Command),
    shell(Command).
