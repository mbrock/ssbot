:- module(nt_hack,
          [ install_relevant_pack/3
          , save_pack_data/0
          , pack_embedding/2
          , known_pack/1,
            relevant_pack/3
          ]).

:- use_module(openai).

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

