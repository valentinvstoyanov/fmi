range(A, A, [A]).
range(A, B, [A|R]) :- A < B, A1 is A + 1, range(A1, B, R).
