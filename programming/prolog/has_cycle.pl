has_cycle(E) :- member([X, Y], E),
				path(E, Y, X, P),
				length(P, L),
				L > 2.
