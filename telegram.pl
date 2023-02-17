:- module(telegram,
          [dump/0,
           telegram/0,
           next_update/0
          ]).

:- use_module(base).
:- use_module(secrets).
:- use_module(apis).
:- use_module(otp).

:- use_module(library(apply_macros), []).
:- use_module(library(persistency)).

:- persistent
       sequence(token:string, id:nonneg).

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

next_update :-
    once(secret(telegram, Token)),
    poll_opts(Token, Opts),
    once(api_post(telegram, ["getUpdates"], json(Opts), Result)),
    debug(telegram, "Result ~w~n", [Result]),
    Result = _{ok: true, result: Updates},
    foreach(
        member(Update, Updates),
        ( save_event(recv(telegram, Update)),
          print_update(Update)
        )),

    findall(I, (member(Update, Updates), I = Update.update_id), Ids),
    max_list(Ids, Max),
    retractall_sequence(Token, _Id),
    debug(telegram, "New sequence: ~w~n", [Max]),
    assert_sequence(Token, Max).

loop :-
    debug(telegram, "Polling...", []),
    foreach(next_update, true),
    loop.

telegram :-
    spin(telegram(poll), loop).

dump :-
    foreach(
        known_event(recv(telegram, Update), T),
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

