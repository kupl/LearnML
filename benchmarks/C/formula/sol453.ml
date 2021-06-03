
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

let rec evalexp x =
	match x with
	| Num y -> y
	| Plus (a, b) -> (evalexp a) + (evalexp b)
	| Minus (a, b) -> (evalexp a) - (evalexp b)

let rec eval a = 
	match a with
	| False -> false
	| True -> true
	| Not x -> not (eval x)
	| AndAlso (x, y) -> (eval x) && (eval y)
	| OrElse (x, y) -> (eval x) || (eval y)
	| Imply (x, y) -> (not (eval x)) || (eval y)
	| Equal (x, y) -> (evalexp x) = (evalexp y)

