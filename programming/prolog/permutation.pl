perm([], []).
perm([X|Xs], Ys) :- perm(Xs, Zs), my_insert(X, Zs, Ys).
