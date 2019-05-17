is_undirected(E) :- not((member([X, Y], E), not(member([Y, X], E)))).
