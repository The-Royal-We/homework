%% Interleave/3 question
interleave([],[],[]).
interleave([],L,L).
interleave(L,[],L).
interleave([H|T], L, [H|R]) :- interleave(T,L,R).
interleave(T, [H|L], [H|R]) :- interleave(T,L,R).

%% Ancestry Question
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
