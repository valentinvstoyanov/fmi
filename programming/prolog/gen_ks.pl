gen_ks(1, S, [S]).
gen_ks(K, S, [Xi|Xs]) :- K > 1,
						 between(0, S, Xi),
						 S1 is S - Xi,
						 K1 is K - 1,
						 gen_ks(K1, S1, Xs).
