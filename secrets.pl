:- module(secrets,
          [ secret/2
          , assert_known_secret/2
          ]).
:- use_module(library(persistency)).

:- persistent
    known_secret(name:atom, secret:text).

:- db_attach("secrets.db", []).

secret_env(openai, "OPENAI_API_KEY").
secret_env(telegram, "TELEGRAM_API_KEY").
secret_env(discord, "DISCORD_BOT_TOKEN").
secret_env(readwise, "READWISE_TOKEN").
secret_env(spotify_id, "SPOTIFY_ID").
secret_env(spotify, "SPOTIFY_SECRET").
secret_env(pirate_weather, "PIRATE_WEATHER_API_KEY").

secret(urbion, "n/a").
secret(qdrant, "n/a").

secret(Name, Secret) :-
    known_secret(Name, Secret).

secret(Name, Secret) :-
    secret_env(Name, Env),
    getenv(Env, Secret).











