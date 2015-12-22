%% Interleave/3 question
interleave([],[],[]).
interleave([],L,L).
interleave(L,[],L).
interleave([H|T], L, [H|R]) :- interleave(T,L,R).
interleave(T, [H|L], [H|R]) :- interleave(T,L,R).

%% Ancestry Question
%%% Defined below for debugging purposes

male(me).
female(sarah).
parent(paul, me).
parent(anne, me).
parent(nora, anne).
parent(noel, sarah).
parent(nora, noel).

grandParent(X, Z) :- parent(X, Y), parent(Y, Z).
sibling(X,Z) :- parent(Y,X), parent(Y,Z).
cousin(X,Z) :- grandParent(Y,X), grandParent(Y,Z).
/*
* Using the above definition, siblings would indeed be delcared cousins,
* as they share the same grandparent which satisfies the preconditions of
* cousin/2. By the same logic I am also a cousin of myself.
*
* One way to avoid this behavior is to define cousin as a relation of my parents
* siblings and the relation of those siblings to the arguments in cousin/2. 
*/
