type expr = 	NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr

type formula = 	TRUE
		| FALSE
		| NOT of formula
		| ANDALSO of formula * formula
		| ORELSE of formula * formula
		| IMPLY of formula * formula
		| LESS of expr * expr

let rec eval (f : formula) : bool = 
	let rec calc (e : expr) : int = match e with
        	| NUM x -> x
	        | PLUS (x, y) -> (calc x) + (calc y)
		| MINUS (x, y) -> (calc x) - (calc y) in
	match f with
		| TRUE -> true
		| FALSE -> false
		| NOT x -> not (eval x)
		| ANDALSO (x, y) -> (eval x) && (eval y)
		| ORELSE (x, y) -> (eval x) || (eval y)
		| IMPLY (x, y) -> (not (eval x)) || (eval y)
		| LESS (x, y) -> (calc x) < (calc y)

