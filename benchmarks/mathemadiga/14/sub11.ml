type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp
;;
exception InvalidExp of string ;;
let rec galculator input = 
	let rec computing (substitution, formula) =
		match formula with
		X -> substitution
		| INT value -> float_of_int value
		| REAL value -> value
		| ADD (first, second) -> (computing (substitution, first)) +. (computing (substitution, second))
  		| SUB (first, second) -> (computing (substitution, first)) -. (computing (substitution, second))
  		| MUL (first, second) -> (computing (substitution, first)) *. (computing (substitution, second))
  		| DIV (first, second) -> (computing (substitution, first)) /. (computing (substitution, second))
  		| SIGMA (first, second, third) -> doSigma (int_of_float (computing (substitution, first)), int_of_float (computing (substitution, second)), third)
  		| INTEGRAL (first, second, third) -> doIntegral (computing (substitution, first), computing (substitution, second), third)
	and
  	doSigma (here, there, formula) =
  		if here > there then 0.0
  		else (computing (float_of_int here, formula)) +. doSigma (here + 1, there, formula)
  	and
  	doIntegral (here2, there2, formula) =
		if here2 +. 0.1 > there2 then 0.0 
  		else (computing (here2, formula)) +. doIntegral (here2 +. 0.1, there2, formula)
  	in
  	match input with
  	X -> raise (InvalidExp "invalid exp")
  	| INT value -> float_of_int value
  	| REAL value -> value
  	| ADD (first, second) -> (galculator first) +. (galculator second)
  	| SUB (first, second) -> (galculator first) -. (galculator second)
  	| MUL (first, second) -> (galculator first) *. (galculator second)
  	| DIV (first, second) -> (galculator first) /. (galculator second)
  	| SIGMA (first, second, third) -> doSigma (int_of_float (galculator first), int_of_float (galculator second), third)
	| INTEGRAL (first, second, third) -> doIntegral (galculator first, galculator second, third)
;;
