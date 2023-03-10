:- module(apis,
          [ api_post/4
          , api_post/3
          , api_get/4,
            api_put/4
          ]).

:- use_module(grok, [hear/1]).
:- use_module(secrets).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(apply_macros)).

bearer_prefix(discord, "Bot").
bearer_prefix(readwise, "Token").
bearer_prefix(_, "Bearer").

base_url(openai, "https://api.openai.com/").
base_url(discord, "https://discord.com/api/v10/").
base_url(urbion, "http://urbion/epap/").
base_url(readwise, "https://readwise.io/api/v2/").
base_url(qdrant, "http://hamlet:6333/").
base_url(etherscan, "https://api.etherscan.io/").

base_url(telegram, URL) :-
    secret(telegram, Token),
    format(string(URL), "https://api.telegram.org/bot~w/", [Token]).

api_url(Service, Path, URL) :-
    \+ Service = telegram,
    base_url(Service, Base),
    string_concat(Base, Path, URL).

api_url(telegram, Path, URL) :-
    base_url(telegram, Base),
    string_concat(Base, Path, URL).

api_auth(Service, Auth) :-
    secret(Service, Token),
    bearer_prefix(Service, Prefix), !,
    atomic_list_concat([Prefix, Token], ' ', Auth).

user_agent(discord, "DiscordBot (https://node.town, 0.5)").
user_agent(_, "node.town").

hush(qdrant).

api_post(Service, PathComponents, Body, Result) :-
    debug(apis, "POST ~w ~w ~w", [Service, PathComponents, Body]),
    atomics_to_string(PathComponents, "/", Path),
    once(api_auth(Service, Auth)),
    api_url(Service, Path, URL),
    user_agent(Service, UserAgent),
    once(http_post(URL, Body, Result,
                   [request_header('Authorization'=Auth),
                    request_header('User-Agent'=UserAgent),
                    json_object(dict)])),
    ( \+ hush(Service) -> 
      hear(post(Service, PathComponents, Body))
    ; true ).

api_post(Service, PathComponents, Result) :-
    api_post(Service, PathComponents, json(_{}), Result).

api_put(Service, PathComponents, Body, Result) :-
    debug(apis, "PUT ~w ~w ~w", [Service, PathComponents, Body]),
    atomics_to_string(PathComponents, "/", Path),
    once(api_auth(Service, Auth)),
    api_url(Service, Path, URL),
    user_agent(Service, UserAgent),
    once(http_put(URL, Body, Result,
                  [request_header('Authorization'=Auth),
                   request_header('User-Agent'=UserAgent),
                   json_object(dict)])),
    ( \+ hush(Service) -> 
      hear(put(Service, PathComponents, Body))
    ; true ).

api_get(Service, PathComponents, QueryParams, Result) :-
    debug(apis, "GET ~w ~w ~w", [Service, PathComponents, QueryParams]),
    user_agent(Service, UserAgent),
    once(api_auth(Service, Auth)),
    uri_query_components(Query, QueryParams),
    atomics_to_string(PathComponents, "/", Path),
    atomics_to_string([Path, Query], "?", PathWithQuery),
    api_url(Service, PathWithQuery, URL),
    debug(http(readwise), "GET ~w", [URL]),
    http_get(URL, Result,
             [request_header('Authorization'=Auth),
              % request_header('User-Agent'=UserAgent),
              json_object(dict)]).

