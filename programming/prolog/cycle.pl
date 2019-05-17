cycle(E, C) :- member([X, Y], E),
				X \= Y,
			   	path(E, Y, X, P),
			   	length(P, N), N > 2,
				C = [X|P].
