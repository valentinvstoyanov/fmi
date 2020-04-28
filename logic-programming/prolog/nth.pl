nth(X, 0, [X|_]).
nth(X, N, [_|T]) :- nth(X, M, T), N is M + 1.
