gen_all_nat_seq([]).
gen_all_nat_seq(L) :- nat(N),
					  between(1, N, K),
					  S is N - K,
					  gen_ks(K, S, L).
