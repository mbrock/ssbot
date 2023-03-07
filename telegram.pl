:- module(telegram, [dump/0, telegram/0]).

:- use_module(grok, [hear/1, past/2]).
:- use_module(secrets).
:- use_module(apis).
:- use_module(otp).

:- use_module(library(apply_macros), []).
:- use_module(library(persistency)).

:- persistent sequence(token:any, id:nonneg).
:- db_attach("telegram.db", []).

poll_opts(Token, Opts) :-
    (   sequence(Token, Next)
    ->  Opts = _{timeout: 60, offset: Next}
    ;   Opts = _{timeout: 60}
    ).

next :-
    once(secret(telegram, Token)),
    poll_opts(Token, Opts),
    once(api_post(telegram, ["getUpdates"], json(Opts), Result)),
    debug(telegram, "Result ~w~n", [Result]),
    Result = _{ok: true, result: Updates},
    foreach(
        member(Update, Updates),
        hear(recv(telegram, Update))),
    bump(Token, Updates).

bump(Token, Updates) :-
    last(Updates, Last),
    Last.get(update_id) = Id,
    Next is Id + 1,
    retractall_sequence(Token, _),
    assert_sequence(Token, Next),
    debug(telegram, "Next: ~w~n", [Next]).

telegram :-
    debug(telegram, "Polling...", []),
    foreach(next, true),
    telegram.

dump :-
    foreach(
        past(recv(telegram, Update), T),
        print_update(T, Update)
    ).

print_update(T, Update) :-
    format_time(string(Time), "%c", T),
    Message = Update.message,
    From = Message.from,
    Chat = Message.chat,
    Chat.type = "group",
    Text = Message.text,
    atomic_list_concat([From.first_name, From.last_name], " ", Name),
    ansi_format([faint], "~w~n", [Time]),
    ansi_format([bold], "(~w) ", [Chat.title]),
    ansi_format([fg(green)], "~w", [Name]),
    ansi_format([faint], ": ", []),
    ansi_format([fg(blue)], "~w~n~n", [Text]).



