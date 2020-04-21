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

let rec value exp =
match exp with
	| Plus(a,b) -> (value a) + (value b)
	| Minus(a,b) -> (value a) - (value b)
	| Num n -> n

let rec eval f =
match f with
	| True -> true
	| False -> false
	| AndAlso(a,b) -> (eval a) && (eval b)
	| OrElse(a,b) -> (eval a) || (eval b)
	| Not a -> not (eval a)
	| Imply(a,b) -> not (eval a) || (eval b)
	| Equal(a,b) -> if (value a) = (value b) then true else false


