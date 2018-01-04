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

let rec cal : expr -> int =
	fun input ->
	match input with
	| NUM a -> a
	| PLUS (a,b) -> cal a + cal b
	| MINUS (a,b) -> cal a - cal b

let rec eval : formula -> bool =
	fun input ->
	match input with
	| TRUE -> true
	| FALSE -> false
	| NOT a -> not (eval a)
	| ANDALSO (a,b) -> (eval a) && (eval b)
	| ORELSE (a,b) -> (eval a ) || (eval b)
	| IMPLY (a,b) -> not ( (eval a) && (not (eval b)  ))
	| LESS (x, y) -> if (cal x < cal y) then eval TRUE else eval FALSE


