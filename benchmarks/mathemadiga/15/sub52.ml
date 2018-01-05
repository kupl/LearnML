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
 

let rec galculatorl exp1 =
	match exp1 with
	| X -> fun x -> x
	| INT i -> fun x -> float_of_int i
	| REAL i -> fun x -> i
	| ADD (i1, i2) -> fun x -> (galculatorl i1 x +. galculatorl i2 x)
	| SUB (i1, i2) -> fun x -> (galculatorl i1 x -. galculatorl i2 x)
	| MUL (i1, i2) -> fun x -> (galculatorl i1 x *. galculatorl i2 x)
	| DIV (i1, i2) -> fun x -> (galculatorl i1 x /. galculatorl i2 x)
	| SIGMA (i1, i2, i3) -> fun x -> (if (galculatorl i1 x) > (galculatorl i2 x) then 0.0
				else galculatorl i3 ((float_of_int (int_of_float (galculatorl i1 x)))) +. 
					galculatorl (SIGMA (INT ((int_of_float (galculatorl i1 x)) + 1) , INT (int_of_float (galculatorl i2 x)), i3 )) x)
	| INTEGRAL (i1, i2, i3) -> fun x -> (if (galculatorl i1 x) > (galculatorl i2 x) 
					then galculatorl ( SUB (REAL 0.0, INTEGRAL (i2, i1, i3))) x
				else if ((galculatorl i1 x) -. (galculatorl i2 x)) < 0.1 then 0.0
				else galculatorl i3 (galculatorl i1 x) +. galculatorl (INTEGRAL ((REAL ((galculatorl i1 x) +. 0.1)), i2 , i3)) x)

let galculator exp1 =
	if galculatorl exp1 1.0 = galculatorl exp1 58.21 then galculatorl exp1 1.0
	else raise (FreeVariable) 

