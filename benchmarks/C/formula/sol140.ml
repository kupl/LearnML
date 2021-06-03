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


let rec value v =
                match v with
                | Num x -> x
                | Plus (x, y) -> (value x) + (value y)
                | Minus (x, y) -> (value x) - (value y)

let rec eval a = 
	match a with
	| True -> true
	| False -> false
	| Not b -> not (eval b)
	| AndAlso(b, c) -> (eval b) && (eval c)
	| OrElse(b, c) -> (eval b) || (eval c)
	| Imply(b, c) -> (not (eval b)) || (eval c)
	| Equal(b, c) -> (value b) = (value c)
