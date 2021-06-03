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

let rec exp e = 
	match e with
		| Num x -> x
		| Plus (x, y) -> (exp x) + (exp y)
		| Minus (x, y) -> (exp x) - (exp y)

let rec eval form = 
	match form with
		| True -> true
		| False -> false
		| Not f -> not (eval f)
		| AndAlso (f1, f2) -> (eval f1) && (eval f2)
		| OrElse (f1, f2) -> (eval f1) || (eval f2)
		| Imply (f1, f2) -> not ((eval f1) && (not (eval f2)))
		| Equal (e1, e2) -> if (exp e1) = (exp e2) then true
				else false
