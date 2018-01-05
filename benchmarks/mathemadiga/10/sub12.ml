type exp = X
		| INT of int
 	  	| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp



exception FreevarError
exception DividedByZero


let rec mathemadiga ex = 
	math ex 0.0 false
	and math ex x var = 
		match ex with
		X -> 
			if not var 
				then raise FreevarError
			else x
		|INT n -> (float_of_int n)
		|REAL f -> f
		|ADD (e1, e2) -> (math e1 x var) +. (math e2 x var)
		|SUB (e1, e2) -> (math e1 x var) -. (math e2 x var)
		|MUL (e1, e2) -> (math e1 x var) *. (math e2 x var)
		|DIV (e1, e2) -> 
			let a = (math e2 x var) in
				if a = 0.0 
					then raise DividedByZero
				else
					( (math e1 x var) /. (math e2 x var) )
		|SIGMA (e1, e2, e3) -> sigma ( (math e1 x var), (math e2 x var), e3)
		|INTEGRAL (e1, e2, e3) ->
			let a = (math e1 x var) in let b = (math e2 x var) in
				if( a > b ) 
					then ((-1.0) *. integral(b, a, e3))
				else 
					integral(a, b, e3)
	
	and sigma (a, b, e) = 
		if a > b 
			then 0.0
		else
			(math e a true) +. sigma ( (a +. 1.0), b, e)
	and integral (a, b, e) = 
		if a >= b
			then 0.0
		else if (a +. 0.1) > b
			then (math e a true) *. (b -. a)
		else
			((math e a true) *. 0.1) +. integral( (a +. 0.1), b, e)
