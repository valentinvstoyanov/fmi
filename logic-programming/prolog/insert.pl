my_insert(X, Ys, Xs) :- append(Y1, Y2, Ys), append(Y1, [X|Y2], Xs).
