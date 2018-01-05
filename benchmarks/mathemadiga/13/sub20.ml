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


let galculator e =
let rec xculator (a,f) =
match (a,f)  with
| ([], X) -> raise FreeVariable
| ((hd::tl), X) -> xculator ([] ,hd)
| (_, INT n) -> float_of_int n
| (_, REAL n) -> n
| (_, ADD (p,q)) -> (xculator (a,p)) +. (xculator (a,q))
| (_, SUB (p,q)) -> (xculator (a,p)) -. (xculator (a,q))
| (_, MUL (p,q)) -> (xculator (a,p)) *. (xculator (a,q))
| (_, DIV (p,q)) -> (xculator (a,p)) /. (xculator (a,q))
| (_, SIGMA (p,q,r)) -> if((xculator (a,q)) -. (xculator (a,p))) >= 0.0
			then (xculator ([REAL (xculator (a, p))], r) 
				+. (xculator (a, (SIGMA ((ADD (p, REAL 1.0)), q, r)))))
			else 0.0
| (_, INTEGRAL (p,q,r)) ->
	if (xculator (a,q)) >= (xculator (a,p))
		then if (xculator (a,q)) -. (xculator (a,p)) >= 0.1
			then (0.1 *. (xculator ([REAL (xculator (a,p))], r)))
				+. (xculator (a, INTEGRAL ((ADD (p, REAL 0.1)), q, r)))
			else 0.0
	else (xculator (a, SUB (REAL 0., INTEGRAL (q,p,r))))
in xculator ([],e)
