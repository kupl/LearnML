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



let rec eval : formula -> bool =
	let rec calculate : exp -> int =
			fun exp ->
				match exp with
					|Plus (a, b) -> (calculate a) + (calculate b)
					|Minus (a, b) -> (calculate a) - (calculate b)
					|Num a -> a in
	fun f ->
		match f with
			| False -> false
			| True -> true
			| AndAlso (a, b) -> (eval a) && (eval b)
			| OrElse (a, b) -> (eval a) || (eval b)
			| Imply (a, b) -> if ((eval a) && (not (eval b))) then false
							  else true
			| Equal (a, b) -> if (calculate a) = (calculate b) then true
							  else false
			| Not a -> not (eval a)

