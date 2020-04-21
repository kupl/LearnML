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

let rec exVal x =
	match x with
		| Num n -> n
		| Plus (a, b) -> (exVal a) + (exVal b)
		| Minus (c, d) -> (exVal c) - (exVal d)

let rec eval f = 
	match f with
		| True -> true
		| False -> false
		| Not n -> if (eval n) = true then false
					else true
		| AndAlso (a, b) -> if ((eval a) = true) && ((eval b) = true) then true
						else false
		| OrElse (c, d) -> if ((eval c) = false) && ((eval d) = false) then false
						else true
		| Imply (e, f) -> if ((eval e) = true) && ((eval f) = false) then false
						else true
		| Equal (g, h) -> if ((exVal g) = (exVal h)) then true
						else false