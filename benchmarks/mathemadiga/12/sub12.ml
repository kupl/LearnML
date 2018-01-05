(*
let INT n =
	int n

let REAL n =
	float n

let ADD(exp1, exp2) =
	float exp1 + exp2

let SUB(exp1, exp2) =
	float exp1 - exp2

let MUL(exp1, exp2) =
	float exp1 * exp2

let DIV(exp1, exp2) =
	if exp2 = 0 then "error"
	else exp1 / exp2 

let rec SIGMA(exp1, exp2, exp3) =
	if (int_of_float exp1) < (int_of_float exp2) then exp3(int_of_float exp1) + SIGMA(((int_of_float exp1)+1), (int_of_float exp2), exp3)
	else if (floor exp1) = (floor exp2) then exp3(floor exp1)
	else 0.0

let temp(exp1, exp2, exp3) =
	if exp1 < exp2 then
		0.1 * exp3(exp1) + temp((exp1+0.1), exp2, exp3)
	else if exp1 = exp2 then 0.1 * exp3(exp1)

let INTEGRAL(exp1, exp2, exp3) =
	if exp1 <= exp2 then temp(exp1, exp2, exp3)
	else (-1) * temp(exp2, exp1, exp3)		 

 *)



 type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
(*| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp 	*)


let rec mathemadiga exp =
try
	match exp with
	| X -> 0.0
	| INT n -> float_of_int n
	| REAL n -> n
	| ADD(exp1, exp2) -> mathemadiga exp1 +. mathemadiga exp2 
	| SUB(exp1, exp2) -> mathemadiga exp1 -. mathemadiga exp2
	| MUL(exp1, exp2) -> mathemadiga exp1 *. mathemadiga exp2
	| DIV(exp1, exp2) -> mathemadiga exp1 /. mathemadiga exp2
(*	| SIGMA(exp1, exp2, exp3) ->
	
		let temp1 = mathemadiga exp1 in
		let temp2 = mathemadiga exp2 in
		let rec innerSigma(exp3, n1, n2) =
			match exp3 with
			| X -> 
				if n1 < n2 then n1 + innerSigma(exp3, (n1+1), n2)
				else if n1 = n2 then n1
				else 0.0
			
			| INT n -> float_of_int n
			| REAL n -> n
			| ADD(exp1, exp2) -> mathemadiga exp1 +. mathemadiga exp2 
			| SUB(exp1, exp2) -> mathemadiga exp1 -. mathemadiga exp2
			| MUL(exp1, exp2) -> mathemadiga exp1 *. mathemadiga exp2
			| DIV(exp1, exp2) ->
						if mathemadiga exp2 = 0.0 then raise
						else mathemadiga exp1 /. mathemadiga exp2	
			| _ -> 0.0 in			
		
		innerSigma(exp3, temp1, temp2)	*)
with Division_by_zero -> 0.0
