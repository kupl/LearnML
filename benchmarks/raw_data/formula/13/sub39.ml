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

let rec makeexp ex =
	match ex with
	| Num i -> i
	| Plus (ex1, ex2) -> (makeexp ex1) + (makeexp ex2)
	| Minus (ex1, ex2) -> (makeexp ex1) - (makeexp ex2)

let rec eval f =
	match f with
	| True -> true
	| False -> false
	| Not f1 -> not (eval f1)
	| AndAlso (f1, f2) -> (eval f1) && (eval f2)
	| OrElse (f1, f2) -> (eval f1) || (eval f2)
	| Imply (f1, f2) ->
		if ((eval f2) = true) && ((eval f2) == true) then true
		else if ((eval f1) = false) then true
		else false
	| Equal (ex1, ex2) -> (makeexp ex1) = (makeexp ex2)


