:- module(otp,
          [
          ]).

:- use_module(library(sweet)).

:- use spawn.
:- use condition.

% I want to implement "structured concurrency" in Prolog.
% I want to be able to write code like this:

example :-
    nest(N, (spin(N, loop(5)),
             spin(N, loop(3)))).

spin(Nest, Goal) :-
    thread_create(Goal, Task, [at_exit(nest(Nest, exit(Task)))]),
    thread_send_message(N, spin(),
    
nest(Nest, Goal) :-
    thread_create(Goal, Nest, [at_exit(nest_exit(Nest))]),
    !,
    thread_join(Nest, Status),
    ( Status = true -> true
    ; Status = false -> fail
    ; throw(Status)
    ).
