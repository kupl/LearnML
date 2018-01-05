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

let rec mygalculator exp var = match exp with
| X -> (match var with
	   | h :: l -> h
	   | [] -> raise FreeVariable)
| INT i -> float_of_int i
| REAL r -> r
| ADD (exp1, exp2) -> (mygalculator exp1 var) +. (mygalculator exp2 var)
| SUB (exp1, exp2) -> (mygalculator exp1 var) -. (mygalculator exp2 var)
| MUL (exp1, exp2) -> (mygalculator exp1 var) *. (mygalculator exp2 var)
| DIV (exp1, exp2) -> (mygalculator exp1 var) /. (mygalculator exp2 var)
| SIGMA (exp1, exp2, exp3) -> let start = float_of_int (int_of_float (mygalculator exp1 var)) 
							and last = float_of_int (int_of_float (mygalculator exp2 var)) in
							if start > last then 0.
							else (mygalculator exp3 [start]) +. mygalculator (SIGMA (REAL (start +. 1.0), REAL last, exp3)) var
| INTEGRAL (exp1, exp2, exp3) -> let start = mygalculator exp1 var and last = mygalculator exp2 var in
								if start > last then -.(mygalculator (INTEGRAL (REAL last, REAL start, exp3)) var)
								else if last -. start < 0.1 then 0.
								else 0.1 *. (mygalculator exp3 [start]) +. (mygalculator (INTEGRAL (REAL (start +. 0.1), REAL last, exp3)) var)

and 
galculator exp = mygalculator exp []
