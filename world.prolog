:- module(world, []).

% Practical reasoning, Aristotelian todos, etc.

wife(ieva).
child(alex).
child(jazi).

have(home).
have(office).
have(X) :- inside(home, X).

inside(P, X) :- in(P, X).
inside(P, X) :- in(P, P2), inside(P2, X).

in(home, fridge).
in(home, freezer).

in(fridge, eggs).
in(fridge, carrots).
in(fridge, potatoes).
in(fridge, beets).
in(fridge, butter).

machine(fridge).
machine(freezer).

food(X) :- in(fridge, X).

love(X) :- wife(X).
love(X) :- child(X).

recipe(omelette) :-
    have(eggs),
    have(butter).

good(praise(X)) :- love(X).
good(maintain(X)) :- have(X), machine(X).
good(eat(X)) :- have(X), food(X).
good(cook(X)) :- recipe(X).
