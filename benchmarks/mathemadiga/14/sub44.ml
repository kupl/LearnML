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

let rec galculator : exp -> float =

	let rec substitute : exp -> float -> exp =
	fun exp t->
		match exp with
			|X -> REAL t
			|INT a -> INT a
			|REAL a -> REAL a
			|ADD (a, b) ->ADD ((substitute a t), (substitute b t)) 
			|SUB (a, b) ->SUB ((substitute a t), (substitute b t))
			|MUL (a, b) ->MUL ((substitute a t), (substitute b t))
			|DIV (a, b) ->DIV ((substitute a t), (substitute b t))
			|_ -> raise FreeVariable  in
	fun exp ->
	match exp with
	|X -> raise FreeVariable
	|INT a -> (float) a
	|REAL a -> a
	|ADD (a, b) -> (galculator a) +. (galculator b) 
	|SUB (a, b) -> (galculator a) -. (galculator b) 
	|MUL (a, b) -> (galculator a) *. (galculator b) 
	|DIV (a, b) -> (galculator a) /. (galculator b) 
	|SIGMA (a, b, c) -> if ((galculator a) > (galculator b)) then 0.
						else ((galculator (substitute c (galculator a))) +. (galculator (SIGMA ((REAL ((galculator a) +. 1.0)), b, c))))

	|INTEGRAL (a, b, c) -> if (abs_float ((galculator a) -. (galculator b))) < 0.1 then 0.
						   else if (galculator a) < (galculator b) then ((galculator (substitute c (galculator a))) *. 0.1 +. (galculator (INTEGRAL ((REAL ((galculator a) +. 0.1)), b, c)))) 
						   else (0. -. (galculator (INTEGRAL(b, a, c))))
