my_reverse(Xs, Zs) :- my_reverse(Xs, [], Zs).

my_reverse([], Ys, Ys).
my_reverse([X|Xs], Ys, Zs) :- L = [X|Ys], my_reverse(Xs, L, Zs).
