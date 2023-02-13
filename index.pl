:- module(nt, []).

:- use_module(secrets).
:- use_module(base).
:- use_module(apis).
:- use_module(discord).

:- use_module(library(thread)).
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(lynx/html_text)).
:- use_module(library(yall)).
:- use_module(library(prolog_pack)).
:- use_module(library(uri)).

:- debug(http(readwise)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

demo(discord) :-
    start(discord, [guilds, guild_messages, guild_message_reactions]).

demo(html, URL) :-
    http_open(URL, Stream, []),
    call_cleanup(
        load_html(Stream, DOM, []),
        close(Stream)),
    html_text(DOM, [width(500)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Readwise
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save_readwise_event(Item) :-
    save_event(readwise(Item)).

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

:- debug(readwise).
