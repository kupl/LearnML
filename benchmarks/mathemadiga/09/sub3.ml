(* Computer Science/2005-11759/Sangcheol Park/Exercise 2-5.*)

exception FreeVariable
exception InvalidSigma
exception DivideByZero

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp
;;

let rec mathemadiga exp =
	let dx = 0.1 in
	let rec x_eval_real(x_formula, x_value) =
		match x_formula with
		| X -> x_value
    | INT a -> float_of_int a
		| REAL a -> a
		| ADD(a, b) -> x_eval_real (a, x_value) +. x_eval_real (b, x_value)
		| SUB(a, b) -> x_eval_real (a, x_value) -. x_eval_real (b, x_value)
		| MUL(a, b) -> x_eval_real (a, x_value) *. x_eval_real (b, x_value)
		| DIV(a, b) ->
				if (x_eval_real (b, x_value) != 0.0)
				then x_eval_real (a, x_value) /. x_eval_real (b, x_value)
				else raise DivideByZero in
	match exp with
	| X -> raise FreeVariable
	| INT a -> float_of_int a
	| REAL a -> a
	| ADD(a, b) -> (mathemadiga a) +. (mathemadiga b)
	| SUB(a, b) -> (mathemadiga a) -. (mathemadiga b)
	| MUL(a, b) -> (mathemadiga a) *. (mathemadiga b)
	| DIV(a, b) ->
			if ((mathemadiga b) != 0.0)
			then (mathemadiga a) /. (mathemadiga b)
			else raise DivideByZero
	| INTEGRAL(a, b, c) ->
			let a_value = mathemadiga a in
			let b_value = mathemadiga b in
			if (a_value > b_value) then 0.0
			else x_eval_real(c, a_value) *. dx +. mathemadiga(INTEGRAL((REAL (a_value +. dx)), REAL b_value, c));
	| SIGMA(a, b, c) ->
			let a_value = mathemadiga a in
			let b_value = mathemadiga b in
			if (a_value > b_value) then 0.0
			else x_eval_real(c, a_value) +. mathemadiga(SIGMA((REAL (a_value +. 1.0)), REAL b_value, c))
;;

(* 
x_eval_real((REAL 5.5), 4.3);;
x_eval_real(ADD((REAL 5.5), X), 4.3);;
x_eval_real(ADD((REAL 5.5), MUL(X, X)), 4.3);;
x_eval_real(ADD((REAL 5.5), MUL(X, ADD(X, INT 5))), 4.3);;
mathemadiga (ADD(INT 1, INT 1));;
mathemadiga (ADD(INT 1, SUB(INT 5, INT 6)));;
mathemadiga (MUL(ADD(INT 5, REAL 3.4), DIV(INT 5, REAL 2.1)));;
mathemadiga (MUL(ADD(INT 5, REAL 3.4), MUL(INT 5, REAL 2.1)));;
mathemadiga (MUL(ADD(INT 5, REAL 3.4), MUL((MUL (REAL 5.75, REAL 34.4)), REAL 2.1)));;
mathemadiga (SIGMA(INT 1, INT 10, INT 5));;
mathemadiga (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 5)));;
mathemadiga (INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1)));;
*)