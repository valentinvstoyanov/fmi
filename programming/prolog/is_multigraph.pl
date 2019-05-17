is_multigraph(E) :- my_remove(Edge, E, E1), member(Edge, E1).
