:- load_library('alice.tuprolog.lib.DCGLibrary').
example(operation(op_add,operation(op_multiply,variable(x),operation(op_subtract,variable(y),variable(z))),const(100))).

lookup(K, [(K, V) | _], V).
lookup(K, [_ | T], V) :- lookup(K, T, V).

variable(Name, variable(Name)).
const(Value, const(Value)).

bool(A) :- A > 0.
not_(A) :- A =< 0.
and(A, B) :- bool(A), bool(B).
or(A, B) :- bool(A); bool(B).
xor(A, B) :- bool(A), not_(B), !; bool(B), not_(A).
impl(A, B) :- not_(A); bool(B).
iff(A, B) :- A > 0, B > 0; A =< 0, B =< 0.

op_add(A, B, operation(op_add, A, B)).
op_subtract(A, B, operation(op_subtract, A, B)).
op_multiply(A, B, operation(op_multiply, A, B)).
op_divide(A, B, operation(op_divide, A, B)).
op_and(A, B, operation(op_and, A, B)).
op_or(A, B, operation(op_o, A, B)).
op_xor(A, B, operation(op_xor, A, B)).
op_impl(A, B, operation(op_impl, A, B)).
op_iff(A, B, operation(op_iff, A, B)).
op_negate(A, operation(op_negate, A)).
op_not(A, operation(op_not, A)).

operation(op_add, A, B, R) :- R is A + B.
operation(op_subtract, A, B, R) :- R is A - B.
operation(op_multiply, A, B, R) :- R is A * B.
operation(op_divide, A, B, R) :- R is A / B.
operation(op_negate, A, R) :- R is -A.
operation(op_and, A, B, R) :- and(A, B), !, R = 1; R = 0.
operation(op_or, A, B, R) :- or(A, B), !, R = 1; R = 0.
operation(op_xor, A, B, R) :- xor(A, B), !, R = 1; R = 0.
operation(op_impl, A, B, R) :- impl(A, B), !, R = 1; R = 0.
operation(op_iff, A, B, R) :- iff(A, B), !, R = 1; R = 0.
operation(op_not, A, R) :- not_(A), !, R = 1; R = 0.

evaluate(const(Value), _, Value).
evaluate(variable(Name), Vars, R) :- atom_chars(Name, [F | _]), lookup(F, Vars, R).
evaluate(operation(Op, A), Vars, R) :-
		evaluate(A, Vars, AV),
		operation(Op, AV, R).
evaluate(operation(Op, A, B), Vars, R) :-
    evaluate(A, Vars, AV),
    evaluate(B, Vars, BV),
    operation(Op, AV, BV, R).

nonvar(V, _) :- var(V).
nonvar(V, T) :- nonvar(V), call(T).

varb([]) --> [].
varb([H | T]) --> {member(H, [x, y, z, 'X', 'Y', 'Z'])}, [H], varb(T).

expr_p(variable(Name)) -->
	{ nonvar(Name, atom_chars(Name, Chars)) },
	varb(Chars),
	{ Chars = [_ | _], atom_chars(Name, Chars) }.

expr_p_rev(variable(Name)) --> expr_p(variable(Name)).

expr_p(const(Value)) -->
  { nonvar(Value, number_chars(Value, Chars)) },
  num(Chars),
  { Chars = [_ | _], number_chars(Value, Chars) }.
expr_p_rev(const(Value)) --> expr_p(const(Value)).

digits_p([]) --> [].
digits_p([H | T]) -->
  { member(H, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']) },
  [H],
  digits_p(T).

num(['-' | T]) --> {T = [_ | _]}, ['-'], digits_p(T).
num(A) --> digits_p(A).

op_p(op_add) --> ['+'].
op_p(op_subtract) --> ['-'].
op_p(op_multiply) --> ['*'].
op_p(op_divide) --> ['/'].
un_op_p(op_not) --> ['!'].
op_p(op_and) --> ['&'], ['&'].
op_p(op_or) --> ['|'], ['|'].
op_p(op_xor) --> ['^'], ['^'].
op_p(op_impl) --> ['-'], ['>'].
op_p(op_iff) --> ['<'], ['-'], ['>'].
un_op_p(op_negate) --> ['n'], ['e'], ['g'], ['a'], ['t'], ['e'].

delete_ws([], []) :- !.
delete_ws([' ' | T], T1) :- !, delete_ws(T, T1).
delete_ws([H | T], [H | T1]) :- delete_ws(T, T1).

expr_p_rev(operation(Op, A, B)) --> ['('], expr_p_rev(A), op_p(Op), expr_p_rev(B), [')'].
expr_p_rev(operation(Op, A)) --> un_op_p(Op), expr_p_rev(A).

expr_p(operation(Op, A, B)) --> ['('], expr_p(A), [' '], op_p(Op), [' '], expr_p(B), [')'].
expr_p(operation(Op, A)) --> un_op_p(Op), [' '], expr_p(A).

infix_str(E, A) :- ground(E), phrase(expr_p(E), C), atom_chars(A, C).
infix_str(E, A) :-   atom(A), atom_chars(A, C), delete_ws(C, C1), phrase(expr_p_rev(E), C1).