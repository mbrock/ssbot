:- module(nt, []).

:- use_module(library(condition)).
:- use_module(library(persistency)).
:- use_module(library(redis)).
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/websocket)).
:- use_module(library(lynx/html_text)).

:- persistent
    known_secret(name:atom, secret:text),
    known_event(data:any, time:float).

:- db_attach("secrets.db", []).

secret_env(openai, "OPENAI_API_KEY").
secret_env(telegram, "TELEGRAM_API_KEY").
secret_env(discord, "DISCORD_BOT_TOKEN").
secret_env(readwise, "READWISE_TOKEN").
secret_env(spotify_id, "SPOTIFY_ID").
secret_env(spotify, "SPOTIFY_SECRET").
secret_env(pirate_weather, "PIRATE_WEATHER_API_KEY").

secret(Name, Secret) :-
    known_secret(Name, Secret).

secret(Name, Secret) :-
    secret_env(Name, Env),
    getenv(Env, Secret).

base_url(openai, "https://api.openai.com/").
base_url(discord, "https://discord.com/api/v10/").

api_url(Service, Path, URL) :-
    base_url(Service, Base),
    string_concat(Base, Path, URL).

api_auth(Service, Auth) :-
    Service = discord,
    secret(Service, Token),
    string_concat("Bot ", Token, Auth).

api_auth(Service, Auth) :-
    secret(Service, Token),
    string_concat("Bearer ", Token, Auth).

user_agent(discord, "DiscordBot (https://node.town, 0.5)").
user_agent(_, "node.town").

api_post(Service, PathComponents, Body, Result) :-
    atomics_to_string(PathComponents, "/", Path),
    api_auth(Service, Auth),
    api_url(Service, Path, URL),
    user_agent(Service, UserAgent),
    http_post(URL, json(Body), Result,
              [request_header('Authorization'=Auth),
               request_header('User-Agent'=UserAgent),
               json_object(dict)]).

api_post(Service, PathComponents, Result) :-
    api_post(Service, PathComponents, _{}, Result).

api_get(Service, PathComponents, Result) :-
    atomics_to_string(PathComponents, "/", Path),
    api_auth(Service, Auth),
    api_url(Service, Path, URL),
    user_agent(Service, UserAgent),
    http_get(URL, Result,
             [request_header('Authorization'=Auth),
              request_header('User-Agent'=UserAgent),
              json_object(dict)]).

completion(Engine, prompt(P), Options, Completion) :-
    put_dict(_{prompt: P}, Options, Options2),
    completion(Engine, Options2, Completion).

completion(Engine, Options, Completion) :-
    api_post(openai, ["v1", "engines", Engine, "completions"], Options, Result),
    member(Completion, Result.choices).

discord_gateway_url(URL) :-
    api_get(discord, ["gateway"], Result),
    string_concat(Result.url, "?v=10&encoding=json", URL).

save_event(X) :-
    get_time(Now),
    assert_known_event(X, Now).

discord_opcode(0, dispatch).
discord_opcode(1, heartbeat).
discord_opcode(7, reconnect).
discord_opcode(9, invalid_session).
discord_opcode(10, hello).
discord_opcode(11, heartbeat_ack).
discord_opcode(2, identify).
discord_opcode(_, _) :- throw(error(bad)).

debug(websocket).
debug(websocket(_)).

discord_gateway_send(Socket, Message) :-
    debug(websocket(send), "Sending ~p", [Message]),
    ws_send(Socket, json(Message)),
    save_event(send(websocket, discord, Message)).

discord_gateway_send(Socket, Op, Data) :-
    discord_opcode(Opcode, Op),
    discord_gateway_send(Socket, _{op: Opcode, d: Data}).

discord_heartbeat(Socket, Seq) :-
    discord_gateway_send(Socket, heartbeat, Seq).

discord_heartbeat(Socket) :-
    discord_heartbeat(Socket, null).

discord_intents(1<<0, guilds).
discord_intents(1<<1, guild_members).
discord_intents(1<<2, guild_bans).
discord_intents(1<<3, guild_emojis).
discord_intents(1<<4, guild_integrations).
discord_intents(1<<5, guild_webhooks).
discord_intents(1<<6, guild_invites).
discord_intents(1<<7, guild_voice_states).
discord_intents(1<<8, guild_presences).
discord_intents(1<<9, guild_messages).
discord_intents(1<<10, guild_message_reactions).
discord_intents(1<<11, guild_message_typing).
discord_intents(1<<12, direct_messages).
discord_intents(1<<13, direct_message_reactions).
discord_intents(1<<14, direct_message_typing).
discord_intents(1<<15, message_content).
discord_intents(1<<16, guild_scheduled_events).

discord_intent_bitmask(Intents, Bitmask) :-
    findall(X, (member(I, Intents), discord_intents(X, I)), Bits),
    sum_list(Bits, Bitmask).

discord_receive(Socket, Message) :-
    ws_receive(Socket, Message, [format(json)]),
    save_event(receive(websocket, discord, Message)).

discord_identify(Socket, Intents) :-
    secret(discord, Token),
    discord_intent_bitmask(Intents, Bitmask),
    discord_gateway_send(
        Socket, identify,
        _{ token: Token,
           intents: Bitmask,
           properties: _{ os: "linux",
                          browser: "node.town",
                          device: "node.town" }}).

discord_gateway_close(Socket) :-
    ws_close(Socket, 1000, "Goodbye"),
    save_event(close(websocket, discord)).

discord_gateway_connect(Intents, Hello, Ready, Socket) :-
    discord_gateway_url(URL), !,
    http_open_websocket(URL, Socket, []),
    discord_receive(Socket, Hello), !,
    discord_heartbeat(Socket), !,
    discord_receive(Socket, _Ack), !,
    discord_identify(Socket, Intents), !,
    discord_receive(Socket, Ready), !,
    discord_gateway_close(Socket), !.

demo(discord) :-
    discord_gateway_connect([guild_messages], Hello, Ready, Socket),
    discord_gateway_close(Socket).

demo(html) :-
    http_open("https://www.gnu.org/", Stream, []),
    call_cleanup(
        load_html(Stream, DOM, []),
        close(Stream)),
    html_text(DOM, [width(72)]).
