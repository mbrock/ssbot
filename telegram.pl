:- module(nt_telegram,
          [next_update/1,
           start/0,
           dump/0
          ]).

:- use_module(base).
:- use_module(secrets).
:- use_module(apis).
:- use_module(otp).

:- use_module(library(apply_macros), []).
:- use_module(library(persistency)).

:- persistent
       sequence(token:atom, id:nonneg).

:- db_attach("telegram.db", []).

poll_timeout(60).

poll_opts(Token, Opts) :-
    sequence(Token, Id), !,
    Next is Id + 1,
    poll_timeout(Timeout),
    Opts = _{timeout: Timeout, offset: Next}.

poll_opts(Token, Opts) :-
    \+ sequence(Token, _Id), !,
    poll_timeout(Timeout),
    Opts = _{timeout: Timeout}.

next_update(Update) :-
    once(secret(telegram, Token)),
    poll_opts(Token, Opts),
    once(api_post(telegram, ["getUpdates"], json(Opts), Result)),
    Result = _{ok: true, result: Updates},
    member(Update, Updates),
    save_event(telegram(Update)),
    retractall_sequence(Token, _Id),
    assert_sequence(Token, Update.update_id).

loop :-
    debug(telegram, "Polling...", []),
    foreach(next_update(Update), print_update(Update)),
    loop.

start :-
    spin(telegram(poll), loop).

dump :-
    foreach(
        known_event(telegram(Update), T),
        print_update(T, Update)
    ).

print_update(Update) :-
    get_time(T),
    print_update(T, Update).

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

