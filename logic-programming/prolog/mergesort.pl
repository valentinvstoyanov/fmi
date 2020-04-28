mergesort([], []).
mergesort([X], [X]).
mergesort([X|Xs], S) :-
    length(Xs, Len),
    0 < Len,
    split_in_half([X|Xs], Ys, Zs),
    mergesort(Ys, SY),
    mergesort(Zs, SZ),
    merge(SY, SZ, S).

merge([], Xs, Xs).
merge(Xs, [], Xs).
merge([X|Xs], [Y|Ys], [X|S]) :-
    X =< Y, merge(Xs, [Y|Ys], S).
merge([X|Xs], [Y|Ys], [Y|S]) :-
    Y =< X, merge([X|Xs], Ys, S).

split_in_half(Xs, Ys, Zs) :-
    length(Xs, Len),
    Half is Len // 2,
    split_at(Xs, Half, Ys, Zs).

split_at(Xs, N, Ys, Zs) :- length(Ys, N), append(Ys, Zs, Xs).
