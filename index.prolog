:- module(nt, []).

:- use_module(library(redis)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

:- redis_server(default, localhost:6379, []).

secret_env(openai, "OPENAI_API_KEY").
secret_env(telegram, "TELEGRAM_API_KEY").
secret_env(discord, "DISCORD_BOT_TOKEN").
secret_env(readwise, "READWISE_TOKEN").
secret_env(spotify_id, "SPOTIFY_ID").
secret_env(spotify_secret, "SPOTIFY_SECRET").
secret_env(pirate_weather, "PIRATE_WEATHER_API_KEY").

secret(Env, Secret) :-
    secret_env(Env, EnvVar),
    getenv(EnvVar, Secret).

base_url(openai, "https://api.openai.com/").

api_url(Service, Path, URL) :-
    base_url(Service, Base),
    string_concat(Base, Path, URL).

api_auth(Service, Auth) :-
    secret(Service, Secret),
    Auth = bearer(Secret).

api_post(Service, PathComponents, Body, Result) :-
    atomics_to_string(PathComponents, "/", Path),
    api_auth(Service, Auth),
    api_url(Service, Path, URL),
    http_post(URL, json(Body), Result, [authorization(Auth), json_object(dict)]).

completion(Engine, Options, Completion) :-
    api_post(openai, ["v1", "engines", Engine, "completions"], Options, Result),
    member(Completion, Result.choices).

