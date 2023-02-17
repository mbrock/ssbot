:- module(otp,
          [ nest/2,
            spin/2,
            wipe/1,
            kill/1,
            lose/0,
            killall/1
          ]).

:- use_module(library(sweet)).
:- use_module(library(spawn)).
:- use_module(library(apply_macros)).

:- dynamic spin/3.

:- meta_predicate
       nest(+, 0),
       spin(+, 0).

nest(Spec, Goal) :-
    cleanup(wipe(Spec)),
    call(Goal).

wipe(Spec) :-
    foreach(join(Spec), true).

kill(Spec) :-
    forall(spin(Spec, thread, Thread),
            (debug(spin, "kill ~q", [Thread]),
             thread_signal(Thread, throw(kill)))),
    !,
    join(Spec).

killall(Spec) :-
    forall(kill(Spec), true).

lose :-
    retractall(spin(_, _, _)).

spin(Name, Goal) :-
    term_to_atom(Name, Atom),
    async(
        (thread_alias(Atom),
         thread_self(Thread),
         assertz(spin(Name, thread, Thread)),
         Goal),
        Token),
    assertz(spin(Name, token, Token)),
    assertz(spin(Name, goal, Goal)),
    debug(spin, "open spin ~q", [Name]).

join(Name) :-
    spin(Name, token, Token),
    debug(spin, "join spin ~q", [Name]),
    Token = ephemeral_token(_, Queue),
    catch(ignore(forall(await(Token), true)),
          E,
          debug(spin, "fail spin ~q: ~q", [Name, E])),
    message_queue_destroy(Queue),
    retractall(spin(Name, _, _)),
    debug(spin, "exit spin ~q", [Name]).

frob(N) :- spin(frob(N), (sleep(N), writeln(N))).

:- begin_tests(nest).

test(foo) :-
    true.
%    nest(frob(_), (frob(1), frob(2))).

:- end_tests(nest).
