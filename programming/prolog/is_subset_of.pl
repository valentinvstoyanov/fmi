is_subset_of(Xs, Ys) :- not((member(X, Xs), not(member(X, Ys)))).
