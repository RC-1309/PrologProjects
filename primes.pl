init(MAX_N) :-	mark_next(4, 2, MAX_N), next_prime(3, MAX_N).

mark_next(Cur, DX, N) :- Cur > N, !.
mark_next(Cur, DX, N) :- assert(table_composite(Cur, DX)), NCur is Cur + DX, mark_next(NCur, DX, N).

next_prime(Cur, N) :- Cur * Cur > N, !.
next_prime(Cur, N) :-
	(composite(Cur); Square is Cur * Cur, mark_compose(Square, Cur, N)),
	NewCur is Cur + 2,
	next_prime(NewCur, N).

mark_compose(Cur, DX, N) :- Cur > N, !.
mark_compose(Cur, DX, N) :-
	composite(Cur), !,
	NewCur is Cur + DX * 2,
	mark_compose(NewCur, DX, N).
mark_compose(Cur, DX, N) :-
	assert(table_composite(Cur, DX)),
	NewCur is Cur + DX * 2,
	mark_compose(NewCur, DX, N).

composite(N) :- table_composite(N, C), !.
prime(N) :- \+ composite(N), N \= 1.

prime_divisors(1, []) :- !.
prime_divisors(N, [N]) :- prime(N), !.
prime_divisors(N, [F | T]) :- number(N), !,
	table_composite(N, X),
    F is X,
    N1 is N / X,
    prime_divisors(N1, T).
prime_divisors(N, Arr) :- multiply(N, Arr).

multiply(N, [N]) :- !, prime(N).
multiply(N, [F, S | T]) :-
	F =< S,
    prime(F),
    multiply(R, [S | T]),
    N is R * F.

concat([], B, B).
concat([H | T], B, [H | R]) :- concat(T, B, R).

compact_prime_divisors(1, []) :- !.
compact_prime_divisors(N, CDs) :- number(N), !,
		prime_divisors(N, [H | T]),
		compact(CDs, H, 1, T).

compact(A, Cur, Num, []) :- !, A = [(Cur, Num)].
compact(A, Cur, Num, [H | T]) :- Cur is H, !, NNum is Num + 1, compact(A, Cur, NNum, T).
compact(A, Cur, Num, [H | T]) :- !, compact(A1, H, 1, T), A2 = [(Cur, Num)], concat(A2, A1, A).

d_d(Cur, [], Ans) :- !, Ans = [Cur].
d_d(Cur, [(N, K) | T], Ans) :- K is 0, !, d_d(Cur, T, Ans).
d_d(Cur, [(N, K) | T], Ans) :- !, d_d(Cur, T, Ans1), K1 is K - 1,
		concat(Cur, [N], Ncur), d_d(Ncur, [(N, K1) | T], Ans3), concat(Ans1, Ans3, Ans).

divisors_divisors(N, Divisors) :- number(N), !,
		compact_prime_divisors(N, Arr),
		d_d([], Arr, Divisors).
