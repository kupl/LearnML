type expr = NUM of int
            | PLUS of expr * expr 
						| MINUS of expr * expr

type formula = TRUE
							| FALSE
							| NOT of formula
							| ANDALSO of formula * formula
							| ORELSE of formula * formula
							| IMPLY of formula * formula
							| LESS of expr * expr

let rec calculator ( n : expr ) : int =
	match n with
	| NUM i -> i
	| PLUS (a, b) -> calculator a + calculator b
	| MINUS (a, b) -> calculator a - calculator b


let rec eval (form : formula) : bool =
	match form with
	| TRUE -> true
	| FALSE -> false
	| NOT form' -> 
		(match (eval form') with
		| true -> false
		| false -> true )
	| ANDALSO (f1, f2) ->
		(match (eval f1) with
		| false -> false
		| true -> eval f2 )
	| ORELSE (f1, f2) ->
		(match (eval f1 ) with
		| true -> true
		| false -> eval f2 )
	| IMPLY (p, q) ->
		(match eval p with 
		| false -> true
		| true -> eval q )
	| LESS (a1, a2) -> 
		(if (calculator a1) - (calculator a2) < 0 then true else false)