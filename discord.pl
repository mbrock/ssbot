:- module(nt_discord, []).

:- use_module(secrets).
:- use_module(base).
:- use_module(apis).

:- use_module(library(http/websocket)).

discord_gateway_url(URL) :-
    api_get(discord, ["gateway"], [], Result),
    string_concat(Result.url, "?v=10&encoding=json", URL).

discord_opcode(0, dispatch).
discord_opcode(1, heartbeat).
discord_opcode(7, reconnect).
discord_opcode(9, invalid_session).
discord_opcode(10, hello).
discord_opcode(11, heartbeat_ack).
discord_opcode(2, identify).
discord_opcode(_, _) :- throw(error(bad)).

:- debug(websocket).
:- debug(websocket(_)).

discord_gateway_send(Socket, Message) :-
    debug(websocket(send), "Sending ~p", [Message]),
    ws_send(Socket, json(Message)),
    save_event(send(websocket, discord, Message)).

discord_gateway_send(Socket, Op, Data) :-
    discord_opcode(Opcode, Op),
    discord_gateway_send(Socket, _{op: Opcode, d: Data}).

:- dynamic discord_sequence/1.

discord_sequence(null).

discord_heartbeat(Socket) :-
    discord_sequence(Seq),
    discord_gateway_send(Socket, heartbeat, Seq).

discord_heartbeat_loop(Socket, Interval) :-
    debug(websocket(heartbeat), "waiting ~p seconds", [Interval]),
    sleep(Interval),
    debug(websocket(heartbeat), "sending heartbeat", []),
    discord_heartbeat(Socket),
    discord_heartbeat_loop(Socket, Interval).

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
    ws_receive(Socket, Payload, [format(json)]),
    Message = Payload.data,
    save_event(receive(websocket, discord, Message)),
    discord_opcode(Op, Message.op),
    (  Op = dispatch
    -> retractall(discord_sequence(_)),
       asserta(discord_sequence(Message.s))
    ;  true
    ).

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

discord_gateway_connect(Intents, Ready, Socket) :-
    discord_gateway_url(URL), !,
    http_open_websocket(URL, Socket, []),
    discord_receive(Socket, Hello), !,

    HeartbeatInterval is Hello.d.heartbeat_interval / 1000,
    thread_create(
        discord_heartbeat_loop(Socket, HeartbeatInterval),
        HeartbeatThread, []),

    thread_at_exit(thread_signal(HeartbeatThread, thread_exit(bye))),

    discord_heartbeat(Socket), !,
    discord_receive(Socket, _Ack), !,
    discord_identify(Socket, Intents), !,
    discord_receive(Socket, Ready), !,
    discord_loop(Socket).

discord_loop(Socket) :-
    discord_receive(Socket, _Message),
    discord_loop(Socket).

%% So, we have a websocket connection to Discord. Now what?
%%
%% We need to process the heartbeats, and we need to process
%% the messages. We can do this with a thread that reads
%% from the socket and puts the messages into a queue, and
%% another thread that reads from the queue and processes
%% the messages.
%%

start(discord, Intents) :-
    thread_create(discord_gateway_connect(Intents, _Ready, _Socket),
                  _,
                  [alias(discord),
                   detached(true)]).