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
	let rec compute a =
	match a with
	| Num a -> a
	| Plus (a,b) -> (compute a) + (compute b)
	| Minus (a,b) -> (compute a) - (compute b)
in 
	match formula with
	| True -> true
	| False -> false 
	| Not a -> not (eval a)
	| AndAlso (a,b) -> (eval a) && (eval b)
	| OrElse (a,b) -> (eval a) || (eval b)
	| Imply (a,b) -> (not (eval a)) || (eval b)
	| Equal (a,b) -> (compute a) = (compute b)