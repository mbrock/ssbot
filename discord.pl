:- module(discord,
          [ discord_session/1,
            discord/0
          ]).

:- use_module(secrets).
:- use_module(grok, [hear/1]).
:- use_module(apis).
:- use_module(otp).

:- use_module(library(http/websocket)).
:- use_module(library(apply_macros), []).
:- use_module(library(sweet)).

:- dynamic discord_sequence/1.

discord_sequence(null).

% 1. receive hello
% 2. start heartbeat
% 3. send identify with intents
% 4. receive ready
% 5. start receiving
% 6. be ready to send??

discord :-
    all_intents(Intents),
    discord_session(Intents).

intent(guilds).
intent(guild_messages).
intent(guild_message_reactions).
intent(direct_messages).
intent(direct_message_reactions).

discord_session(Intents) :-
    discord_connect(Socket, Pulse),

    spin(discord_pulse,
         discord_heartbeat_loop(Socket, Pulse)),

    once(discord_identify(Socket, Intents)),
    once(discord_receive(Socket, _Ready)),

    spin(discord_receive,
         discord_receive_loop(Socket)),

    writeln(waiting).

all_intents(Intents) :-
    findall(I, intent(I), Intents).

discord_connect(Socket, Pulse) :-
    discord_gateway_url(URL),
    http_open_websocket(URL, Socket, []),
    once(discord_receive(Socket, Hello)),
    discord_heartbeat_interval(Hello, Pulse),
    once(discord_heartbeat(Socket)).

discord_gateway_url(URL) :-
    api_get(discord, ["gateway"], [], Result),
    string_concat(Result.url, "?v=10&encoding=json", URL).

discord_receive(Socket, Message) :-
    ws_receive(Socket, Payload, [format(json)]),
    Message = Payload.data,
    hear(recv(discord, Message)),
    discord_opcode(Message.op, Op),
    (  Op = dispatch
    -> retractall(discord_sequence(_)),
       asserta(discord_sequence(Message.s))
    ;  true
    ).

discord_receive_loop(Socket) :-
    once(discord_receive(Socket, _Message)),
    discord_receive_loop(Socket).

discord_heartbeat_interval(Hello, Seconds) :-
    Seconds is (Hello.d.heartbeat_interval / 1000).

discord_heartbeat(Socket) :-
    discord_sequence(Seq),
    discord_gateway_send(Socket, heartbeat, Seq).
    
discord_heartbeat_loop(Socket, Interval) :-
    sleep(Interval),
    once(discord_heartbeat(Socket)),
    debug(discord, "heartbeat", []),
    discord_heartbeat_loop(Socket, Interval).

discord_gateway_send(Socket, Message) :-
    ws_send(Socket, json(Message)),
    hear(send(discord, Message)).

discord_gateway_send(Socket, Op, Data) :-
    once(discord_opcode(Opcode, Op)),
    discord_gateway_send(Socket, _{op: Opcode, d: Data}).

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
    hear(close(discord)).

discord_opcode(0, dispatch).
discord_opcode(1, heartbeat).
discord_opcode(7, reconnect).
discord_opcode(9, invalid_session).
discord_opcode(10, hello).
discord_opcode(11, heartbeat_ack).
discord_opcode(2, identify).

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
    findall(X, (member(I, Intents),
                discord_intents(X, I)), Bits),
    sum_list(Bits, Bitmask).

grok:dull(heartbeat, send(discord, X)) :-
    discord_opcode(X.op, heartbeat).

grok:dull(heartbeat, recv(discord, X)) :-
    discord_opcode(X.op, heartbeat_ack).
