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

let rec eval formula =
	let rec solve exp = 
		match exp with
		| Num a -> a
		| Plus (a, b) -> (solve a) + (solve b)
		| Minus (a, b) -> (solve a) - (solve b) in
	match formula with
	| True -> true
	| False -> false
	| Not a -> if (eval a) = true then false else true
	| AndAlso (a, b) -> if (eval a) = true && (eval b) = true then true else false
	| OrElse (a, b) -> if (eval a) = false && (eval b) = false then false else true
	| Imply (a, b) -> if (eval a) = false then true else if (eval b) = true then true else false
	| Equal (a, b) -> if (solve a) = (solve b) then true else false 
