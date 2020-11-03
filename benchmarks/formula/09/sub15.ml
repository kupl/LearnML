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

let rec eval f =
	let rec eval2 e =
		match e with (Num n) -> n
			| (Plus (n1, n2)) -> (eval2 n1) + (eval2 n2)
			| (Minus (n1, n2)) -> (eval2 n1) - (eval2 n2) in
	match f with True -> true
			| False -> false
			| (Not f2) -> (eval f2)
			| (AndAlso (f1, f2)) -> (eval f1) & (eval f2)
			| (OrElse (f1, f2)) -> (eval f1) || (eval f2)
			| (Imply (f1, f2)) -> (if (eval f1) then (eval f2) 
						else true)
			| (Equal (e1, e2)) -> (eval2 e1) = (eval2 e2)