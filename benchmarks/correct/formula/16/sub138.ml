type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec eval : formula -> bool
= fun f -> 
	match f with
	|	True -> true
	|	False -> false
	|	Not p -> not (eval p)
	|	AndAlso (p, q) -> (eval p) && (eval q)
	|	OrElse (p, q) -> (eval p) || (eval q)
	|	Imply (p, q) -> if eval p then eval q else true
	|	Equal (a, b) -> 
			let rec calc e =
				match e with
				|	Num i -> i
				|	Plus (i, j) -> (calc i) + (calc j)
				|	Minus (i, j) -> (calc i) - (calc j) in
			(calc a) = (calc b)