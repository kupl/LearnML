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

let rec galculator (e: exp) =
	let rec formula_change (num, formula) = 
		match formula with 
		| X -> REAL num
		| INT i -> formula
		| REAL f -> formula 
		| ADD (e1, e2) -> ADD(formula_change (num, e1), formula_change(num, e2))
		| SUB (e1, e2) -> SUB(formula_change (num, e1), formula_change(num, e2))
		| MUL (e1, e2) -> MUL(formula_change (num, e1), formula_change(num, e2))
		| DIV (e1, e2) -> DIV(formula_change (num, e1), formula_change(num, e2))
		| SIGMA (e1, e2, e3) -> SIGMA(formula_change (num, e1), formula_change(num, e2), e3)
		| INTEGRAL (e1, e2, e3) -> INTEGRAL(formula_change(num, e1), formula_change(num, e2), e3)
	in
	let sigma ((start: int), (final: int), (formula: exp)) = 
		let rec sigma_sub ((cur: int), (n: int), (f: exp)) = 
			if (cur = n)
				then (galculator (formula_change(float_of_int(cur), f)))
			else	 
				(galculator (formula_change(float_of_int(cur), f))) +. (sigma_sub ((cur + 1), n, f))
		in
		if (start > final) then 0.0
		else (sigma_sub (start, final, formula))
	in
	let integral ((start: float), (final: float), (formula: exp)) = 
		let rec integral_sub ((cur: float), (n: float), (f: exp)) = 
			if (cur +. 0.1 >= n)
				then 0.0
			else
				((galculator (formula_change(cur, f))) *. 0.1) +. (integral_sub ((cur +. 0.1), n, f))
		in
		if (start < final) 
			then (integral_sub (start, final, formula))
		else
			0.0 -. (integral_sub (final, start, formula))
	in
	match e with
	| INT i -> float_of_int(i)
	| REAL f -> f
	| ADD (e1, e2) -> if (e1 = X || e2 = X) then raise FreeVariable
					  else (galculator e1) +. (galculator e2)	
	| SUB (e1, e2) -> if (e1 = X || e2 = X) then raise FreeVariable
					  else (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> if (e1 = X || e2 = X) then raise FreeVariable
					  else (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> if (e1 = X || e2 = X) then raise FreeVariable
					  else (galculator e1) /. (galculator e2)
	| SIGMA (e1, e2, e3) -> (sigma (int_of_float(galculator(e1)), int_of_float(galculator(e2)), e3))
	| INTEGRAL (e1, e2, e3) -> (integral (galculator(e1), galculator(e2), e3))
	| X -> raise FreeVariable
	
