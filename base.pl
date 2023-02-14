:- module(nt_base,
          [ known_event/2
          , save_event/1
          ]).

:- use_module(library(persistency)).

:- persistent
    known_event(data:any, time:float).

:- db_attach("events.db", []).

save_event(X) :-
    get_time(Now),
    assert_known_event(X, Now),
    debug(event, "~p", [X]).
