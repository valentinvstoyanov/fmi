my_member(X, [X]).
my_member(X, [X|_]).
my_member(X, [Y|Ys]) :- X \= Y, member(X, Ys).

my_member2(X, L) :- append(_, [X|_], L).
