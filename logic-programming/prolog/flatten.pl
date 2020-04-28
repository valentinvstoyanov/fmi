flatten([], []).
flatten(X, [X]) :- not(is_list(X)).
flatten([X|Xs], Ys) :- flatten(X, FX), flatten(Xs, FXs), append(FX, FXs, Ys).
