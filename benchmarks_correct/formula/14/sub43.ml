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

let rec get_value: expr -> int =
	fun e_in ->
		match e_in with
		| NUM i -> i
		| PLUS (e1, e2) -> (get_value e1) + (get_value e2)
		| MINUS (e1, e2) -> (get_value e1) - (get_value e2)

let rec eval: formula -> bool =
	fun f_in ->
		match f_in with
		| TRUE -> true
		| FALSE -> false
		| NOT f -> not (eval f)
		| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
		| ORELSE (f1, f2) -> (eval f1) || (eval f2)
		| IMPLY (f1, f2) -> not (eval f1) || (eval f2)
		| LESS (e1, e2) ->
			if (get_value e1) < (get_value e2) then true
			else false
