(*
 *  Programming Languages, 2013 Fall.
 *	department : computer science & engineering
 *	student ID : 2012-11242 / name : Seon-bi, Park
 *)

type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

type binding = UNBIND
			| BIND of float

exception FreeVariable

let rec calculator (e, b) =              (* exp * binding -> float *)
    match (e, b) with
	| (X, UNBIND) -> raise FreeVariable
	| (X, BIND(f)) -> f
	| (INT i,_) -> (float_of_int) i
    | (REAL f,_) -> f
	| (ADD (e1, e2),_) ->
		(
		 match (e1, e2) with
		 | (INT x, INT y) -> float_of_int (x + y)
		 | (_,_) -> (calculator (e1, b) +. calculator (e2, b))
		)
    | (SUB (e1, e2),_) ->
		(
		 match (e1, e2) with
         | (INT x, INT y) -> float_of_int (x - y)
         | (_,_) -> (calculator (e1, b) -. calculator (e2, b))
		) 
	| (MUL (e1, e2),_) ->
		(
		 match (e1, e2) with
         | (INT x, INT y) -> float_of_int (x * y)
         | (_,_) -> (calculator (e1, b) *. calculator (e2, b))
		)
    | (DIV (e1, e2),_) ->
		(
		 match (e1, e2) with
         | (INT x, INT y) -> float_of_int (x / y)
         | (_,_) -> (calculator (e1, b) /. calculator (e2, b))
		)
    | (SIGMA (e1, e2, e3),_) ->
		let x = (int_of_float (calculator (e1, b)))
		and y = (int_of_float (calculator (e2, b))) in
		(
		 if (x > y) then 0.0
		 else if (x = y) then calculator (e3, BIND(float_of_int x))
		 else calculator (e3, BIND(float_of_int x)) +. calculator (SIGMA(INT (x+1), INT y, e3), b)
		)
	| (INTEGRAL (e1, e2, e3),_) ->
		let x = calculator (e1, b)
		and y = calculator (e2, b) in
		(
		 if (x > y) then (-1.)*.(calculator (INTEGRAL(REAL y, REAL x, e3), b))
		 else if ((x = y)||((y -. x) < 0.1)) then 0.0
		 else ((calculator (e3, BIND(x)))*.0.1) +. (calculator (INTEGRAL(REAL (x+.0.1), REAL y, e3), b))
		)

let galculator e =					(* exp -> float *)
	calculator (e, UNBIND)
