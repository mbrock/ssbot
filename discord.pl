:- module(nt_discord,
          [ start/2
          ]).

:- use_module(secrets).
:- use_module(base).
:- use_module(apis).

:- use_module(library(http/websocket)).
:- use_module(library(spawn)).
:- use_module(library(apply_macros)).

:- debug(websocket).
:- debug(websocket(_)).

:- dynamic discord_sequence/1.

discord_sequence(null).

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

discord_gateway_send(Socket, Message) :-
    debug(websocket(send), "Sending ~p", [Message]),
    ws_send(Socket, json(Message)),
    save_event(send(websocket, discord, Message)).

discord_gateway_send(Socket, Op, Data) :-
    once(discord_opcode(Opcode, Op)),
    discord_gateway_send(Socket, _{op: Opcode, d: Data}).

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

:- debug(event).

discord_receive(Socket, Message) :-
    ws_receive(Socket, Payload, [format(json)]),
    Message = Payload.data,
    save_event(receive(websocket, discord, Message)),
    discord_opcode(Message.op, Op),
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

discord_heartbeat_interval(Hello, Seconds) :-
    Seconds is (Hello.d.heartbeat_interval / 1000).

discord_heartbeat(Socket) :-
    discord_sequence(Seq),
    discord_gateway_send(Socket, heartbeat, Seq).

% 1. receive hello
% 2. start heartbeat
% 3. send identify with intents
% 4. receive ready
% 5. start receiving
% 6. be ready to send??
%
discord_heartbeat_loop(Socket, Interval) :-
    debug(websocket(heartbeat), "waiting ~p seconds", [Interval]),
    sleep(Interval),
    debug(websocket(heartbeat), "sending heartbeat", []),
    once(discord_heartbeat(Socket)),
    discord_heartbeat_loop(Socket, Interval).

discord_receive_loop(Socket) :-
    once(discord_receive(Socket, _Message)),
    discord_receive_loop(Socket).

discord_session(Intents) :-
    discord_gateway_url(URL),
    http_open_websocket(URL, Socket, []),
    once(discord_receive(Socket, Hello)),
    once(discord_heartbeat(Socket)),
    discord_heartbeat_interval(Hello, Seconds),
    async(discord_heartbeat_loop(Socket, Seconds), Heartbeat),
    once(discord_identify(Socket, Intents)),
    once(discord_receive(Socket, _Ready)),
    async(discord_receive_loop(Socket), Receive),
    writeln(waiting),
    await(Heartbeat),
    await(Receive).

start(discord, Intents) :-
    discord_session(Intents).










