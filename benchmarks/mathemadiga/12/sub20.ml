(* 2009-11718 2-1*)

exception InvalidSigma
exception FreeVariable

type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

let rec mm exp =
		match exp with
		| X -> X
		| INT a -> (REAL (float_of_int a))
		| REAL a -> REAL a
		| ADD (ex1, ex2) -> ADD ((mm ex1), (mm ex2))
		| SUB (ex1, ex2) -> SUB ((mm ex1), (mm ex2))
		| MUL (ex1, ex2) -> MUL ((mm ex1), (mm ex2))
		| DIV (ex1, ex2) -> DIV ((mm ex1), (mm ex2))
		| SIGMA (a, b, ex) -> (sigma (INT (int_of_float (calc (mm a))), INT (int_of_float (calc (mm b))), ex))
		| INTEGRAL (a, b, ex) -> (integral ((mm a), (mm b), ex))

	and calc exp =
		match exp with
		| X -> raise FreeVariable
		| INT n -> (float_of_int n)
		| REAL n -> n	
		| ADD (ex1, ex2) -> (calc ex1) +. (calc ex2)	
		| SUB (ex1, ex2) -> (calc ex1) -. (calc ex2)
		| MUL (ex1, ex2) -> (calc ex1) *. (calc ex2)
		| DIV (ex1, ex2) -> (calc ex1) /. (calc ex2)
		| SIGMA (n, m, ex2) -> (calc (mm (SIGMA (INT (int_of_float (calc (mm n))), INT (int_of_float (calc (mm m))), (mm ex2)))))
		| INTEGRAL (n, m, ex2) -> (calc (mm (INTEGRAL ((mm n), (mm m), (mm ex2)))))  

	and sigma (a, b, exp) =
		if (mmm a) > (mmm b) then raise InvalidSigma
		else if (mmm a) =(mmm b) then REAL (calc (sCalc ((mmm a), exp)))
		else REAL (calc (ADD((sigma (REAL (calc (ADD ((mm a), REAL 1.))), (mm b), exp)), (REAL (calc (sCalc ((mmm a), exp))))) )) 
			
	and sCalc (a, ex) =
		match ex with
		| X -> REAL a
		| INT n -> REAL (float_of_int n)
		| REAL n -> REAL n
		| ADD (ex1, ex2) -> REAL (calc (ADD ((sCalc (a, ex1)), (sCalc (a, ex2)))))
		| SUB (ex1, ex2) -> REAL (calc (SUB ((sCalc (a, ex1)), (sCalc (a, ex2)))))
		| MUL (ex1, ex2) -> REAL (calc (MUL ((sCalc (a, ex1)), (sCalc (a, ex2)))))
		| DIV (ex1, ex2) -> REAL (calc (DIV ((sCalc (a, ex1)), (sCalc (a, ex2)))))
		| SIGMA (n, m, ex2) -> (sigma ((REAL (calc (sCalc (a, (mm n))))), (REAL (calc (sCalc (a, (mm m))))), ex2))
		| INTEGRAL (n, m, ex2) -> (integral ((REAL (calc (sCalc (a, (mm n))))), (REAL (calc (sCalc (a, (mm m))))), ex2)) 
				
	and integral (a, b, exp) =
		if (mmm a) = (mmm b) then (REAL 0.)
		else if ((mmm a) > (mmm b)) then MUL ((integral (b, a, exp)), (REAL (-1.)))
		else if ((mmm a) < (mmm (SUB (b, REAL 0.1)))) 
			then REAL (calc (ADD (REAL (calc(MUL ((REAL (calc (iCalc ((mmm a), exp)))), REAL 0.1))), 
				(integral (REAL (calc (ADD ((mm a), REAL 0.1))), (mm b), exp)))))
		else MUL ((REAL (calc (iCalc ((mmm a), exp)))), SUB ((mm b), (mm a))) 	
	
	and iCalc (a, ex) =
		match ex with
		| X -> REAL a
		| INT n -> REAL (float_of_int n)
		| REAL n -> REAL n
		| ADD (ex1, ex2) -> REAL (calc (ADD ((iCalc (a, ex1)), (iCalc (a, ex2)))))
		| SUB (ex1, ex2) -> REAL (calc (SUB ((iCalc (a, ex1)),(iCalc (a, ex2)))))
		| MUL (ex1, ex2) -> REAL (calc (MUL ((iCalc (a, ex1)), (iCalc (a, ex2)))))
		| DIV (ex1, ex2) -> REAL (calc (DIV ((iCalc (a, ex1)),  (iCalc (a, ex2)))))
		| SIGMA (n, m, ex2) -> (sigma ((INT (int_of_float (calc (iCalc (a, (mm n)))))), (INT (int_of_float (calc (iCalc (a, (mm m)))))), ex2))
		| INTEGRAL (n, m, ex2) ->  (integral ((REAL (calc (iCalc (a, (mm n))))), (REAL (calc (iCalc (a, (mm m))))), ex2))	
										

and mmm exp =
	(calc (mm exp))	
let mathemadiga exp =
	(mmm exp)

