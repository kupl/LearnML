type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp;;

let rec eval fin = 
	match fin with
	| True -> true
	| False -> false
	| Not f1 -> not (eval f1)
	| AndAlso (f1, f2) -> (eval f1) && (eval f2)
	| OrElse (f1, f2) -> (eval f1) || (eval f2)
	| Imply (f1, f2) -> if (eval f1) = false then true
						else if (eval f1) = (eval f2) then true
						else false
	| Equal (e1, e2) ->
		let rec calc exp =
			match exp with
			| Num x -> x
			| Plus (e1, e2) -> (calc e1) + (calc e2)
			| Minus (e1, e2) -> (calc e1) - (calc e2) in
		if (calc e1) = (calc e2) then true
		else false;;
