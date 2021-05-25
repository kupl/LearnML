type exp = Num of int
			| Plus of exp * exp
			| Minus of exp * exp

type formula = True
			| False
			| Not of formula
			| AndAlso of formula * formula
			| OrElse of formula * formula
			| Imply of formula * formula
			| Equal of exp * exp

let rec cal : exp -> int =
	fun input ->
	match input with
	| Num a -> a
	| Plus (a,b) -> cal a + cal b
	| Minus (a,b) -> cal a - cal b

let rec eval : formula -> bool =
	fun input ->
	match input with
	| True -> true
	| False -> false
	| Not a -> not (eval a)
	| AndAlso (a,b) -> (eval a) && (eval b)
	| OrElse (a,b) -> (eval a ) || (eval b)
	| Imply (a,b) -> not ( (eval a) && (not (eval b)  ))
	| Equal (x, y) -> if (cal x = cal y) then eval True else eval False


