exception Error
exception DividedByZero
exception SigmaError

type exp = X
	 | INT of int
	 | REAL of float
	 | ADD of exp * exp
	 | SUB of exp * exp
	 | MUL of exp * exp
	 | DIV of exp * exp
	 | SIGMA of exp * exp * exp
	 | INTEGRAL of exp * exp * exp

let mathemadiga input =

	let rec math (ex, s) =
	
		let rec integral2(p, q, e, f) =
                	if((q -. p) <= 0.1) then (math(e, (p::f)) *. (q -. p))
        	        else ((math(e, (p::f)) *. 0.1) +. (integral2((p +. 0.1), q, e, f)))
	        in
	
		match (ex, s) with
			| (X, []) -> raise(Error)
			| (X, hd::tl) -> hd
			| (INT a, _) -> (float_of_int a)
			| (REAL a, _) -> a
			| (ADD(a, b), f) -> (math(a,f) +. (math(b,f)))
			| (SUB(a, b), f) -> (math(a,f) -. (math(b,f)))
			| (MUL(a, b), f) -> (math(a,f) *. (math(b,f)))
			| (DIV(a, b), f) -> (if (math(b,f) = 0.0) then raise(DividedByZero)
					     else (math(a,f) /. (math(b,f))))
			| (SIGMA(a, b, c), f) -> (if (math(a,f) > (math(b,f))) then raise(SigmaError)
						  else if ((math(b,f) -. math(a,f)) < 1.0) then (math(c, (math(a,f)::f)))
						  else (math(c,math(a,f)::f) +. (math (SIGMA(REAL (math(a,f) +.1.0), REAL (math(b,f)), c), f))))
			| (INTEGRAL(a, b, c), f) -> (if (math(a,f) > math(b,f)) then (integral2(math(b,f), math(a,f), c, f))
						     else (integral2(math(a,f), math(b,f), c, f)))
	in
	math(input, [])
