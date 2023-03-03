:- module(readwise,
          [ readwise_item/1
          , save_readwise_embeddings/0,
            save_readwise_event/1,
            readwise_export/0,
            etherscan/2
          ]).

:- use_module(grok).
:- use_module(apis).
:- use_module(openai).
:- use_module(secrets).

:- debug(http(readwise)).

save_readwise_event(Item) :-
    hear(readwise(Item)).

readwise_export(start, Items, NextCursor) :-
    api_get(readwise, ["export"], [], Export),
    NextCursor = cursor(Export.nextPageCursor),
    Items = Export.results,
    forall(member(Item, Items), save_readwise_event(Item)).

readwise_export(cursor(Cursor), Items, NextCursor) :-
    api_get(readwise, ["export"], [pageCursor=Cursor], Export),
    (  Export.nextPageCursor = null
    -> NextCursor = end
    ;  NextCursor = cursor(Export.nextPageCursor)
    ),
    Items = Export.results,
    forall(member(Item, Items), save_readwise_event(Item)).

readwise_export_loop(Cursor, Items) :-
    readwise_export(Cursor, Items1, NextCursor),
    (   NextCursor = cursor(_)
    ->  readwise_export_loop(NextCursor, Items2),
        append(Items1, Items2, Items)
    ;   true).

readwise_item(Item) :-
    readwise_export_loop(start, Items),
    member(Item, Items).

readwise_export :-
    readwise_export_loop(start, _Items).

save_readwise_embeddings :-
    ChunkSize = 25,
    findnsols(
        ChunkSize, Text,
        ( known_event(readwise(Item), _T),
          member(Highlight, Item.highlights),
          Highlight.text = Text,
          \+ known_embedding(Text, _)),
        Chunk
    ),
    debug(readwise, "Saving ~w embeddings", [ChunkSize]),
    embeddings(Chunk, _).

