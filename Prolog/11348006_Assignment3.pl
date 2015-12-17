interleave([],[],[]).
interleave([],L,L).
interleave(L,[],L).
interleave([H|T], L, [H|R]) :- interleave(T,L,R).
interleave(T, [H|L], [H|R]) :- interleave(T,L,R).
