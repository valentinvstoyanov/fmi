is_complete([V|E]) :-
			not((member(X, V), member(Y, V), not(member([X, Y], E)))).
