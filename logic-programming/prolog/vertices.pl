add_vertex(V, VL, VR) :- not(member(V, VL)), append(VL, [V], VR).
add_vertex(V, VL, VL) :- member(V, VL).

vertices([], []).
vertices([[X,Y]|Es], Vs) :- vertices(Es, EsV),
							add_vertex(X, EsV, XV),
							add_vertex(Y, XV, Vs).
