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

let rec pushX : exp * float -> float
= fun (e, f) -> match e with  X -> f
| INT n -> float_of_int n
| REAL f -> f
| ADD (a, b) -> (pushX (a,f)) +. (pushX (b,f))
| SUB (a, b) -> (pushX (a,f)) -. (pushX (b,f))
| MUL (a, b) -> (pushX (a,f)) *. (pushX (b,f))
| DIV (a, b) -> (pushX (a,f)) /. (pushX (b,f))
| SIGMA (a, b, c) -> let x = (int_of_float (pushX (a,f))) in
				     let y = (int_of_float (pushX (b,f))) in 
					if x <= y then (pushX (c, (float_of_int x))) +. (pushX (SIGMA(INT (x+1), INT y, c),f)) else 0.0
| INTEGRAL (a, b, c) -> let x = (pushX (a,f)) in
						let y = (pushX (b,f)) in
					if x > y then (pushX ((INTEGRAL (b, a, c)),f)) *. -1.0
					else if x +. 0.1 > y then 0.0
					else ((pushX (c, x)) *. 0.1) +. (pushX ((INTEGRAL (REAL (x +. 0.1), (REAL y), c)),f))

let rec galculator : exp -> float
= fun e -> match e with X -> raise FreeVariable
| INT n -> float_of_int n
| REAL f -> f
| ADD (a,b) -> (galculator a) +.(galculator b)
| SUB (a,b) -> (galculator a) -. (galculator b)
| MUL (a,b) -> (galculator a) *. (galculator b)
| DIV (a,b) -> (galculator a) /. (galculator b)
| SIGMA (a, b, c) -> let x = (int_of_float (galculator a)) in
				     let y = (int_of_float (galculator b)) in 
					if x <= y then (pushX (c, (float_of_int x))) +. (galculator (SIGMA(INT (x+1), INT y, c))) else 0.0
| INTEGRAL (a, b, c) -> let x = (galculator a) in
						let y = (galculator b) in
					if x > y then (galculator (INTEGRAL (b, a, c))) *. -1.0
					else if x +. 0.1 > y then 0.0
					else ((pushX (c, x)) *. 0.1) +. (galculator (INTEGRAL (REAL (x +. 0.1), (REAL y), c)))
					