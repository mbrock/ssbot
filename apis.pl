:- module(nt_apis,
          [ api_post/4
          , api_post/3
          , api_get/4
          ]).

:- use_module(base).
:- use_module(secrets).

:- use_module(library(http/json)).
:- use_module(library(http/http_client)).

bearer_prefix(discord, "Bot").
bearer_prefix(readwise, "Token").
bearer_prefix(_, "Bearer").

base_url(openai, "https://api.openai.com/").
base_url(discord, "https://discord.com/api/v10/").
base_url(urbion, "http://urbion/epap/").
base_url(readwise, "https://readwise.io/api/v2/").

api_url(Service, Path, URL) :-
    base_url(Service, Base),
    string_concat(Base, Path, URL).

api_auth(Service, Auth) :-
    secret(Service, Token),
    bearer_prefix(Service, Prefix), !,
    atomic_list_concat([Prefix, Token], ' ', Auth).

user_agent(discord, "DiscordBot (https://node.town, 0.5)").
user_agent(_, "node.town").

api_post(Service, PathComponents, Body, Result) :-
    atomics_to_string(PathComponents, "/", Path),
    api_auth(Service, Auth),
    api_url(Service, Path, URL),
    user_agent(Service, UserAgent),
    http_post(URL, Body, Result,
              [request_header('Authorization'=Auth),
               request_header('User-Agent'=UserAgent),
               json_object(dict)]),
    save_event(post(Service, PathComponents, Body)).

api_post(Service, PathComponents, Result) :-
    api_post(Service, PathComponents, json(_{}), Result).

api_get(Service, PathComponents, QueryParams, Result) :-
    user_agent(Service, UserAgent),
    api_auth(Service, Auth),
    uri_query_components(Query, QueryParams),
    atomics_to_string(PathComponents, "/", Path),
    atomics_to_string([Path, Query], "?", PathWithQuery),
    api_url(Service, PathWithQuery, URL),
    debug(http(readwise), "GET ~w", [URL]),
    http_get(URL, Result,
             [request_header('Authorization'=Auth),
              request_header('User-Agent'=UserAgent),
              json_object(dict)]).
