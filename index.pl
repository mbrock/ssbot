:- module(nt,
          [ eink_show/1
          , demo/1
          , demo/2
          ]).

:- use_module(base).
:- use_module(apis).
:- use_module(discord).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), []).

:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(lynx/html_text)).

eink_show(latex(Latex)) :-
    api_post(urbion, ["DISPLAY-LATEX"], text(Latex), _).

intent(guilds).
intent(guild_messages).
intent(guild_message_reactions).
intent(direct_messages).
intent(direct_message_reactions).

all_intents(Intents) :-
    findall(I, intent(I), Intents).

demo(discord) :-
    all_intents(Intents),
    discord_session(1, Intents).

demo(html, URL) :-
    http_open(URL, Stream, []),
    call_cleanup(
        load_html(Stream, DOM, []),
        close(Stream)),
    html_text(DOM, [width(500)]).

message(receive(websocket, discord, X), Time, Message) :-
    is_dict(X),
    X.t = "MESSAGE_CREATE",
    Content = X.content,
    ChannelId = X.channel_id,
    Author = X.author.



