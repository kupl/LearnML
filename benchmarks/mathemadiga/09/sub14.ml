(* 2006-11782 Song Young-chan, HW2-5, mathemadiga *)

exception FreeVariable of string
exception InvalidSigma of string
exception DivideByZero of string

type exp = X
	 | INT of int
	 | REAL of float
	 | ADD of exp * exp
	 | SUB of exp * exp
	 | MUL of exp * exp
	 | DIV of exp * exp
	 | SIGMA of exp * exp * exp
	 | INTEGRAL of exp * exp * exp

let rec mathemadiga expression =
	match expression with
	  X -> raise (FreeVariable "X is freevariable")
	| INT(number) -> (float_of_int number)
	| REAL(number) -> number
	| ADD(first, second) -> ((mathemadiga first) +. (mathemadiga second))
	| SUB(first, second) -> ((mathemadiga first) -. (mathemadiga second))
	| MUL(first, second) -> ((mathemadiga first) *. (mathemadiga second))
	| DIV(first, second) -> if ((mathemadiga second) = 0.0) then raise (DivideByZero "second operand is Zero.")
					else ((mathemadiga first) /. (mathemadiga second))
	| SIGMA(start, last, expr) ->
		let rec find_cal (exp,k) =
			match (exp,k) with
			  (X,k) -> mathemadiga(k)
			| (INT(number), k) -> mathemadiga(INT(number))
			| (REAL(number), k) -> mathemadiga(REAL(number))
			| (ADD(first, second), k) -> ((find_cal (first, k)) +. (find_cal (second, k)))
			| (SUB(first, second), k) -> ((find_cal (first, k)) -. (find_cal (second, k)))
			| (MUL(first, second), k) -> ((find_cal (first, k)) *. (find_cal (second, k)))
			| (DIV(first, second), k) -> if ((find_cal (second, k)) = 0.0) then raise (DivideByZero "second operand is Zero")
						     else ((find_cal (first, k)) /. (find_cal (second, k)))
			| (SIGMA(start, last, expre), k) -> (mathemadiga (SIGMA (REAL(find_cal (start, k)), REAL(find_cal (last, k)), expre)))
			| (INTEGRAL(start, last, expre), k) -> (mathemadiga (INTEGRAL (REAL(find_cal (start, k)), REAL(find_cal (last, k)), expre))) in
		let low = mathemadiga (start) in
		let high = mathemadiga (last) in
			if (low > high) then raise (InvalidSigma "Invalid input, Lower bound > Upper bound")
			else if ((low <= high) && (high -. low >= 1.0)) then ((find_cal (expr, start)) +. (mathemadiga (SIGMA(REAL(low+.1.0), last, expr))))
			else (find_cal (expr, start))
	| INTEGRAL(start, last, expr) -> 
		let rec find_cal (exp,k) =
			match (exp,k) with
			  (X,k) -> mathemadiga(k)
			| (INT(number), k) -> mathemadiga(INT(number))
			| (REAL(number), k) -> mathemadiga(REAL(number))
			| (ADD(first, second), k) -> ((find_cal (first, k)) +. (find_cal (second, k)))
			| (SUB(first, second), k) -> ((find_cal (first, k)) -. (find_cal (second, k)))
			| (MUL(first, second), k) -> ((find_cal (first, k)) *. (find_cal (second, k)))
			| (DIV(first, second), k) -> if ((find_cal (second, k)) = 0.0) then raise (DivideByZero "second operand is Zero")
						     else ((find_cal (first, k)) /. (find_cal (second, k)))
			| (SIGMA(start, last, expre), k) -> (mathemadiga (SIGMA (REAL(find_cal (start, k)), REAL(find_cal (last, k)), expre)))
			| (INTEGRAL(start, last, expre), k) -> (mathemadiga (INTEGRAL (REAL(find_cal (start, k)), REAL(find_cal (last, k)), expre))) in
		let low = mathemadiga (start) in
		let high = mathemadiga (last) in
			if ((low < high) && ((high -. low) >= 0.1)) then ((find_cal (expr, start)) *. 0.1 +. (mathemadiga (INTEGRAL(REAL(low +. 0.1), last, expr))))
			else if (low < high) then ((find_cal (expr, start)) *. (high -. low))
			else if ((high < low) && ((low -. high) >= 0.1)) then ((find_cal (expr, start)) *. (-0.1) +. (mathemadiga (INTEGRAL(REAL(low -. 0.1), last, expr))))
			else if (high < low) then ((find_cal (expr, start)) *. (high -. low))
			else 0.0
