type formula = True | False
			| Not of formula
			| AndAlso of formula * formula
			| OrElse of formula * formula
			| Imply of formula * formula
			| Equal of exp * exp
and exp = Num of int
		| Plus of exp * exp
		| Minus of exp * exp

let rec cal e =
	match e with
	|Num x -> x
	|Plus (a, b) -> cal a + cal b
	|Minus (a, b) -> cal a - cal b

let rec eval f = 
	match f with
	|True -> true
	|False -> false
	|Not a -> not(eval a)
	|AndAlso (a, b) -> (eval a) && (eval b)
	|OrElse (a, b) -> (eval a) || (eval b)
	|Imply (a, b) -> (not(eval a)) || (eval b)
	|Equal (a, b) -> (cal a) = (cal b)

