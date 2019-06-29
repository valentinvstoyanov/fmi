%is list
is_list([]).
is_list([_|_]).

%append
appnd([], Ys, Ys).
appnd([X|Xs], Ys, [X|Zs]) :- appnd(Xs, Ys, Zs).

%member
membr(X, [X|_]).
membr(X, [Y|Ys]) :- X \= Y, membr(X, Ys).
membr2(X, Ys) :- appnd(_, [X|_], Ys).

%last
lst(X, [X]).
lst(X, [_|Xs]) :- lst(X, Xs).
lst2(X, Xs) :- appnd(_, [X], Xs).

%reverse
rev(Xs, Ys) :- rev(Xs, [], Ys).
rev([], Xs, Xs).
rev([X|Xs], Ys, Zs) :- rev(Xs, [X|Ys], Zs).

%prefix
prefix(Xs, Ys) :- appnd(Xs, _, Ys).

%suffix
suffix(Xs, Ys) :- appnd(_, Xs, Ys).

%infix
infix(Xs, Ys) :- prefix(Zs, Ys), suffix(Xs, Zs).

%insert
insrt(X, Xs, Ys) :- appnd(X1s, X2s, Xs), appnd(X1s, [X|X2s], Ys).

%remove
remv(X, Xs, Ys) :- appnd(X1s, [X|X2s], Xs), appnd(X1s, X2s, Ys).

%permutation
perm([], []).
perm([X|Xs], Ys) :- perm(Xs, Zs), insert(X, Zs, Ys).

%is sorted
is_sorted([]).
is_sorted([_]).
is_sorted([X1, X2|Xs]) :- X1 =< X2, is_sorted([X2|Xs]).
is_sorted2(Xs) :- not((append(_, [X1, X2|_], Xs), X1 > X2)).

%sort
srt(Xs) :- perm(Xs, Ys), is_sorted(Ys).

%quick sort
partition(_, [], [], []).
partition(Pivot, [X|Xs], [X|Ls], Rs) :- X < Pivot, partition(Pivot, Xs, Ls, Rs).
partition(Pivot, [X|Xs], Ls, [X|Rs]) :- X >= Pivot, partition(Pivot, Xs, Ls, Rs).
qs([], []).
qs([X|Xs], Ys) :- partition(X, Xs, Ls, Rs), qs(Ls, SLs), qs(Rs, SRs), appnd(SLs, [X|SRs], Ys). 

%subset
subset([], []).
subset([X|Xs], [X|Ys]) :- subset(Xs, Ys).
subset([_|Xs], Ys) :- subset(Xs, Ys).

%is subset of
is_subset(Xs, Ys) :- not((membr(X, Xs), not(membr(X, Ys)))).

%nth element
nth(X, 0, [X|_]).
nth(X, N, [_|Xs]) :- N > 0, N1 is N - 1, nth(X, N1, Xs).

%in union
in_union(A, Xs, Ys) :- member(A, Xs); member(A, Ys).

%in intersection
in_intersec(A, Xs, Ys) :- member(A, Xs), member(A, Ys).

%in difference
in_diff(A, Xs, Ys) :- member(A, Xs), not(member(A, Ys)).

%equal
equal(Xs, Ys) :- is_subset(Xs, Ys), is_subset(Ys, Xs).

%remove duplicates
remv_dupli([], []).
remv_dupli([X|Xs], [X|Ys]) :- remv_dupli(Xs, Ys), not(member(X, Ys)).
remv_dupli([X|Xs], Ys) :- remv_dupli(Xs, Ys), member(X, Ys).

%palindrom
pali([]).
pali([_]).
pali([X|Xs]) :- appnd([X|Ys], [X], [X|Xs]), pali(Ys).
pali2(Xs) :- rev(Xs, Ys), Xs = Ys.

%add vertices
add_vertex(X, Vs, Vs) :- member(X, Vs).
add_vertex(X, Vs, [X|Vs]) :- not(member(X, Vs)).

%extract vertices
vertices([], []).
vertices([[X, Y]|Es], Vs) :- vertices(Es, V1s), add_vertex(X, V1s, XV1s), add_vertex(Y, XV1s, Vs).
