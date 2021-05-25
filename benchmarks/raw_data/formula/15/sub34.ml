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

let rec calculator ( n : exp ) : int =
	match n with
	| Num i -> i
	| Plus (a, b) -> calculator a + calculator b
	| Minus (a, b) -> calculator a - calculator b


let rec eval (form : formula) : bool =
	match form with
	| True -> true
	| False -> false
	| Not form' -> 
		(match (eval form') with
		| true -> false
		| false -> true )
	| AndAlso (f1, f2) ->
		(match (eval f1) with
		| false -> false
		| true -> eval f2 )
	| OrElse (f1, f2) ->
		(match (eval f1 ) with
		| true -> true
		| false -> eval f2 )
	| Imply (p, q) ->
		(match eval p with 
		| false -> true
		| true -> eval q )
	| Equal (a1, a2) -> 
		(if (calculator a1) - (calculator a2) = 0 then true else false)