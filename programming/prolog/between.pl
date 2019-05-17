between(A, B, A) :- A =< B.
between(A, B, R) :- A < B, A1 is A + 1, between(A1, B, R).
