:- module(nt, []).

:- use_module(apis).
:- use_module(discord).

:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(lynx/html_text)).

eink_show(latex(Latex)) :-
    api_post(urbion, ["DISPLAY-LATEX"], text(Latex), _).

intent(guilds).
intent(guild_messages).
intent(guild_message_reactions).

all_intents(Intents) :-
    findall(I, intent(I), Intents).

demo(discord) :-
    all_intents(Intents),
    discord_session(Intents).

demo(html, URL) :-
    http_open(URL, Stream, []),
    call_cleanup(
        load_html(Stream, DOM, []),
        close(Stream)),
    html_text(DOM, [width(500)]).


