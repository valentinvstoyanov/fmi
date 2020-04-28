int(Z) :- nat(N), switch_sign(N, Z).

switch_sign(0, 0).
switch_sign(X, Y) :- X > 0, (Y is X; Y is -X).
