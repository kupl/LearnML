type exp =
	X
	|INT of int
	|REAL of float
	|ADD of exp * exp
	|SUB of exp * exp
	|MUL of exp * exp
	|DIV of exp * exp
	|SIGMA of exp * exp * exp
	|INTEGRAL of exp * exp * exp
exception InvalidInput of string

let rec mathemadiga ex =
	
	let rec substitute(value, exp) =
		match exp with
		X -> value
		|INT a -> (float)a
		|REAL a -> a
		|ADD (a, b) -> (substitute(value, a)) +. (substitute(value, b))
		|SUB (a, b) -> (substitute(value, a)) -. (substitute(value, b))
		|MUL (a, b) -> (substitute(value, a)) *. (substitute(value, b))
		|DIV (a, b) -> (substitute(value, a)) /. (substitute(value, b))
		|_ -> 0.0
	in

	match ex with
	X -> raise (InvalidInput "not static value")
	|INT a -> (float)a
	|REAL a -> a
	|ADD (a, b) -> (mathemadiga a) +. (mathemadiga b)
	|SUB (a, b) -> (mathemadiga a) -. (mathemadiga b)
	|MUL (a, b) -> (mathemadiga a) *. (mathemadiga b)
	|DIV (a, b) -> (mathemadiga a) /. (mathemadiga b)
	|SIGMA (INT a, INT b, f) ->
		if(a > b) then 0.0
		else substitute((float)a, f) +. mathemadiga(SIGMA (INT (a+1), INT b, f))
	|INTEGRAL (REAL a, REAL b, f) ->
		if((a -. b) > 0.01) then 0.0
		else substitute(a, f) +. mathemadiga(INTEGRAL (REAL (a+.0.1), REAL b, f))
