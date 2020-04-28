split([], []).
split(L, [A|R]) :- append(A, B, L), A \= [], split(B, R).
