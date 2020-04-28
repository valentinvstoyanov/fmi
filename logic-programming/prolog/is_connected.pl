is_connected([V|E]) :- 
				not((member(X, V), member(Y, V), not(path(E, X, Y, _)))).
