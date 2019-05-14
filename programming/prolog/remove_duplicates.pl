remove_duplicates([], []).
remove_duplicates([X|Xs], [X|Ys]) :- remove_duplicates(Xs, Ys), not(member(X, Ys)).
remove_duplicates([X|Xs], Ys) :- remove_duplicates(Xs, Ys), member(X, Ys).
