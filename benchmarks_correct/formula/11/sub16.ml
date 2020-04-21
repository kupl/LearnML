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


let rec eval form = 
	let rec exval exp =
		match exp with
		Num a -> a
		| Plus (e1, e2) -> (exval e1)+(exval e2)
		| Minus (e1, e2) -> (exval e1)-(exval e2)
	in

	match form with
	True -> true
	| False -> false
	| Not f -> (not (eval f))
	| AndAlso (f1, f2) -> (eval f1) && (eval f2)
	| OrElse (f1, f2) -> (eval f1) || (eval f2)
	| Imply (f1, f2) -> (not (eval f1)) || (eval f2)
	| Equal (e1, e2) -> if (exval e1)=(exval e2) then true
						else false
