type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr



let rec eval : formula -> bool =
	let rec calculate : expr -> int =
			fun exp ->
				match exp with
					|PLUS (a, b) -> (calculate a) + (calculate b)
					|MINUS (a, b) -> (calculate a) - (calculate b)
					|NUM a -> a in
	fun f ->
		match f with
			| FALSE -> false
			| TRUE -> true
			| ANDALSO (a, b) -> (eval a) && (eval b)
			| ORELSE (a, b) -> (eval a) || (eval b)
			| IMPLY (a, b) -> if ((eval a) && (not (eval b))) then false
							  else true
			| LESS (a, b) -> if (calculate a) < (calculate b) then true
							  else false
			| NOT a -> not (eval a)

