exception InvalidSigma
exception DivideByZero
exception FreeVariable

type exp = X
|INT of int
|REAL of float
|ADD of exp * exp
|SUB of exp * exp
|MUL of exp * exp
|DIV of exp * exp
|SIGMA of exp * exp * exp
|INTEGRAL of exp * exp * exp

let rec mathemadiga e =
	let rec func (f, (x:float)) =
		match f with
		|X -> REAL(x)
		|INT i -> INT i
		|REAL r -> REAL r
		|ADD(a, b) -> ADD(func(a, x), func(b, x))
		|SUB(a, b) -> SUB(func(a, x), func(b, x))
		|MUL(a, b) -> MUL(func(a, x), func(b, x))
		|DIV(a, b) -> DIV(func(a, x), func(b, x))
		|SIGMA(c, d, g) -> 
			if mathemadiga(c) < mathemadiga(d) then ADD((func(g, mathemadiga(c))), (SIGMA(ADD(c, REAL 1.0), d, g)))
			else if mathemadiga(c) = mathemadiga(d) then func(g, mathemadiga(c))
			else raise InvalidSigma
		|INTEGRAL(c, d, g) ->
			if mathemadiga(d) -. mathemadiga(c) > 0.1 then 
				ADD(MUL(REAL 0.1, func(g, mathemadiga(c))), (INTEGRAL(ADD(c, REAL 0.1), d, g)))
			else if mathemadiga(d) -. mathemadiga(c) <= 0.1 && mathemadiga(d) -. mathemadiga(c) >= 0.0 then
				MUL(SUB(func(d, x), func(c, x)), func(g, mathemadiga(c)))
			else (*if mathemadiga(d) -. mathemadiga(c) < 0.0 then*)
				MUL(REAL (-1.0), func(INTEGRAL(d, c, g), x))
	in

	match e with
	|X -> raise FreeVariable
	|INT i -> float_of_int i
	|REAL r -> r
	|ADD(a, b) -> mathemadiga(a) +. mathemadiga(b)
	|SUB(a, b) -> mathemadiga(a) -. mathemadiga(b)
	|MUL(a, b) -> mathemadiga(a) *. mathemadiga(b)
	|DIV(a, b) -> 
		if mathemadiga(b) = 0.0 then raise DivideByZero
		else mathemadiga(a) /. mathemadiga(b)
	|SIGMA(a, b, f) ->
		if mathemadiga(a) < mathemadiga(b) then mathemadiga(func(f, mathemadiga(a))) 
												+. mathemadiga(SIGMA(ADD(a, REAL 1.0), b, f))
		else if mathemadiga(a) = mathemadiga(b) then mathemadiga(func(f, mathemadiga(a)))
		else raise InvalidSigma
	|INTEGRAL(a, b, f) ->
		if mathemadiga(b) -. mathemadiga(a) > 0.1 then (0.1 *. mathemadiga(func(f, mathemadiga(a))))
														+. mathemadiga(INTEGRAL(ADD(a, REAL 0.1), b, f))
		else if mathemadiga(b) -. mathemadiga(a) <= 0.1 && mathemadiga(b) -. mathemadiga(a) >= 0.0 then 
				((mathemadiga(b)-.mathemadiga(a)) *. mathemadiga(func(f, mathemadiga(a))))
		else (*if mathemadiga(b) -. mathemadiga(a) < 0.0 then*)
				mathemadiga(MUL(INT(-1), INTEGRAL(b, a, f)))
				
