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

%simple path
simple_path(Es, X, Y, P) :- sp(Es, X, Y, [], P).
sp(_, Y, Y, Visited, Visited).
sp(Es, W, Y, Visited, P) :- W \= Y, member([W, X], Es), not(member(X, Visited)), appnd(Visited, [X], XVisited), sp(Es, X, Y, XVisited, P). 

%is connected
is_connected([Vs, Es]) :- not((member(X, Vs), member(Y, Vs), not(simple_path(Es, X, Y, _)))).

%is complete
is_complete([Vs, Es]) :- not((member(X, Vs), member(Y, Vs), not(member([X, Y], Es)))).

%is undirected
is_undirected(Es) :- not((member([X, Y], Es), not(member([Y, X], Es)))).

%is directed
is_directed(Es) :- not(is_undirected(Es)).

%is multi graph
is_multi(Es) :- remv(E, Es, EEs), not(member(E, EEs)).

%has cycle
has_cycle(Es) :- member([X, Y], Es), X \= Y, simple_path(Es, Y, X, P), length(P, N), N > 2.

%clique
clique([Vs, Es], Cs) :- subset(Cs, Vs), is_complete([Cs, Es]).

%max clique
max_clique([Vs, Es], Cs, K) :- clique([Vs, Es], Cs), length(Cs, K), not((clique([Vs, Es], C1s), length(C1s, K1), K1 > K)).

%is cut vertex
is_cut([Vs, Es], X) :- remv(X, Vs, XVs), not(is_connected([XVs, Es])).

%natural numbers generator
nat(0).
nat(N) :- nat(N1), N is N1 + 1.

%pair of natural numbers
nat_pair(A, B) :- nat(N), between(0, N, A), B is N - A.

%integer generator
int(Z) :- nat(N), switch_sign(N, Z).
switch_sign(0, 0).
switch_sign(A, B) :- A > 0, (B is A; B is -A).

%between - generate number in [A, B]
between(A, B, A) :- A =< B. 
between(A, B, C) :- A < B, A1 is A + 1, between(A1, B, C).

%range - generate list of numbers in [A, B]
range(A, A, [A]).
range(A, B, [A|Xs]) :- A < B, A1 is A + 1, range(A1, B, Xs).

%generate K numbers with sum S
gen_ks(1, S, [S]).
gen_ks(K, S, [Xi|Xs]) :- K > 1, between(0, S, Xi), SXi is S - Xi, K1 is K - 1, gen_ks(K1, SXi, Xs).

%generate all finite lists of natural numbers
gen_all_nat_seq([]).
gen_all_nat_seq(Xs) :- nat(N), between(1, N, K), S is N - K, gen_ks(K, S, Xs).

%flatten
flatten([], []).
flatten(X, [X]) :- not(is_list(X)).
flatten([X|Xs], Ys) :- flatten(X, FX), flatten(Xs, FXs), append(FX, FXs, Ys).

%split
splt([], []).
splt(Xs, [Y|Ys]) :- append(Y, Zs, Xs), Y \= [], splt(Zs, Ys).

%count
cnt([], _, 0).
cnt([X|Xs], X, N) :- cnt(Xs, X, N1), N is N1 + 1.
cnt([X|Xs], Y, N) :- X \= Y, cnt(Xs, Y, N).

%fibonacci
fib(N) :- fib(N, _).
fib(0, 1).
fib(X, Y) :- fib(W, X), Y is W + X.

%most frequent(least frequent is the same, just change the sign).
mf(X, N, Xs) :- member(X, Xs), cnt(Xs, X, N), not((member(X1, Xs), cnt(Xs, X1, N1), N < N1)).
