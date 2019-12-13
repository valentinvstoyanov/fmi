% Takes two lists and joins them together.
concat([], Y, Y).
concat([X|Xs], Y, [X|Z]) :- concat(Xs, Y, Z).

% The idea is to generate permutation P1, then observe that P1 = P2 o P3,
% and then finally P can be expressed as P = P2 o [X|P3]
perm([], []).
perm([X|Xs], P) :- perm(Xs, P1), concat(P2, P3, P1),
    concat(P2, [X|P3], P).
