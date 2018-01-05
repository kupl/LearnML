type exp = X
	|	INT of int
	|	REAL of float
	|	ADD of exp * exp
	|	SUB of exp * exp
	|	MUL of exp * exp
	| 	DIV of exp * exp
	|	SIGMA of exp * exp * exp
	|	INTEGRAL of exp * exp * exp


type flag = ON | OFF

exception FreeVariable
exception InvalidSigma
exception DIVBYZERO
let rec subDiga expr sgnl vlu =
	match expr, sgnl with
		X , ON	-> vlu
	|	X , OFF -> raise FreeVariable
	|	INT n, _ -> float_of_int n
	|	REAL r, _-> r
	|	ADD (oprd1, oprd2), _ -> (subDiga oprd1 sgnl vlu) +. (subDiga oprd2 sgnl vlu)
	|	SUB (oprd1, oprd2), _ -> (subDiga oprd1 sgnl vlu) -. (subDiga oprd2 sgnl vlu)
	|	MUL (oprd1, oprd2), _ -> (subDiga oprd1 sgnl vlu) *. (subDiga oprd2 sgnl vlu)
	| 	DIV (oprd1, oprd2), _ -> (let dvdr = (subDiga oprd2 sgnl vlu) in
					if (dvdr = 0.0) then raise DIVBYZERO 
					else (subDiga oprd1 sgnl vlu) /. dvdr)

	|	SIGMA (INT oprd1, INT oprd2, oprd3), _ -> if (oprd1 > oprd2) then 0.0
							else if (oprd1 = oprd2) then (subDiga oprd3 ON (float_of_int oprd1))
							else (subDiga oprd3 ON (float_of_int oprd1)) +.
							(subDiga (SIGMA ((INT (oprd1+1)), (INT oprd2), oprd3)) sgnl vlu)
	|	SIGMA (exp1, exp2, exp3), _ -> (let oprd1 = (int_of_float (subDiga exp1 sgnl vlu)) in
										let oprd2 = (int_of_float (subDiga exp2 sgnl vlu)) in
										subDiga (SIGMA ((INT oprd1), (INT oprd2), exp3)) OFF 0.0 )
	|	INTEGRAL (oprd1, oprd2, oprd3), _ -> (let bott = (subDiga oprd1 sgnl vlu) in
						let upp = (subDiga oprd2 sgnl vlu) in
						if bott > upp then 0.0 -. (subDiga (INTEGRAL (oprd2, oprd1, oprd3)) sgnl vlu)
						else if (upp -. bott) > 0.1 
						then (((subDiga oprd3 ON bott) *. 0.1) +.
								(subDiga (INTEGRAL (REAL (bott+. 0.1), (REAL upp), oprd3)) sgnl vlu))
						else if bott = upp then 0.0
						else (upp -. bott) *. (subDiga oprd3 ON bott))


let mathemadiga expr =
	(subDiga expr OFF 0.0)



