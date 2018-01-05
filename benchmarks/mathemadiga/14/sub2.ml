(* 2006-11377 hw2-4 *)

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreeVariable

let galculator exp =
	let rec gal (e, valueList) = 
		match e with
		| X -> (
			match valueList with
			| [] -> raise FreeVariable
			| top::rest -> top
		)
		| INT i -> (float_of_int i)
		| REAL f -> f
		| ADD (e1, e2) -> gal(e1,valueList) +. gal(e2,valueList)
		| SUB (e1, e2) -> gal(e1,valueList) -. gal(e2,valueList)
		| MUL (e1, e2) -> gal(e1,valueList) *. gal(e2,valueList)
		| DIV (e1, e2) -> gal(e1,valueList) /. gal(e2,valueList)
		| SIGMA (e1, e2, e3) -> 
			let rec sigma (start, finish, exp, sum) =
				if start > finish then 
					sum
				else
					sigma(start+1, finish, exp, sum+.gal(exp, (float_of_int start)::valueList))
			in
			let start = int_of_float (gal(e1,valueList)) in
			let finish = int_of_float (gal(e2,valueList)) in
			sigma (start, finish, e3, 0.0)
		| INTEGRAL (e1, e2, e3) -> 
			let rec integral (start, finish, exp, sum) =
				if (finish-.start) < 0.1 then 
					sum
				else
					integral(start+.0.1, finish, exp, sum+.gal(exp, start::valueList)*.0.1)
			in
			let start = gal(e1, valueList) in
			let finish = gal(e2, valueList) in
			if start > finish then
				-.integral (finish, start, e3 , 0.0)
			else
				integral (start, finish, e3, 0.0)
	in
	gal (exp, [])
	