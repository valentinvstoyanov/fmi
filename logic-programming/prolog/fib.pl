fib(N, F) :- fib(N, 0, 1, F).

fib(0, A, _, A).
fib(N, A, B, F) :- N > 0, N1 is N - 1, B1 is A + B, fib(N1, B, B1, F). 
