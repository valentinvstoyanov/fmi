is_sorted([]).
is_sorted([_]).
is_sorted([X, Y|Zs]) :- X =< Y, is_sorted([Y|Zs]).
