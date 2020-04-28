path(E, X, Y, P) :- path(E, X, Y, [X], P).

path(_, Y, Y, Vis, Vis).
path(E, X, Y, Vis, P) :- X \= Y, member([X, W], E),
						not(member(W, Vis)), append(Vis, [W], VisW),
						path(E, W, Y, VisW, P).
