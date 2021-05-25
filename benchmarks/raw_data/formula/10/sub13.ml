type formula = True
			 | False
			 | Not of formula
			 | AndAlso of formula * formula
			 | OrElse of formula * formula
			 | Imply of formula * formula
			 | Equal of exp * exp
	and exp = Num of int
			| Plus of exp * exp
			| Minus of exp * exp

let imply a b = 
	if a && not b 
		then false
	else
		true

let rec eval form = 
	match form with
	True -> true
	|False -> false
	|Not f -> not (eval f)
	|AndAlso (f1, f2) -> (eval f1) && (eval f2)
	|OrElse (f1, f2) -> (eval f1) || (eval f2)
	|Imply (f1, f2) -> imply (eval f1) (eval f2)
	|Equal (e1, e2) -> 
		if (exp e1) = (exp e2)
			then true
		else
			false
	and exp e =
		match e with
		|Num n -> n
		|Plus (e1, e2) -> (exp e1) + (exp e2)
		|Minus (e1, e2) -> (exp e1) - (exp e2)
