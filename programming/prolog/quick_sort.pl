qs([], []).
qs([H|T], R) :- partition(H, T, Xs, Ys),
				qs(Xs, SXs),
			   	qs(Ys, SYs),
			   	append(SXs, [H|SYs], R).

partition(_, [], [], []).
partition(P, [H|T], [H|Xs], Ys) :- H < P, partition(P, T, Xs, Ys).
partition(P, [H|T], Xs, [H|Ys]) :- H >= P, partition(P, T, Xs, Ys).
