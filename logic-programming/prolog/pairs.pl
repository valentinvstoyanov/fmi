pairs(A, B) :- nat(N), N > 0, between(0, N, A), B is N - A.
