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

let rec value exp =
match exp with
	| PLUS(a,b) -> (value a) + (value b)
	| MINUS(a,b) -> (value a) - (value b)
	| NUM n -> n

let rec eval f =
match f with
	| TRUE -> true
	| FALSE -> false
	| ANDALSO(a,b) -> (eval a) && (eval b)
	| ORELSE(a,b) -> (eval a) || (eval b)
	| NOT a -> not (eval a)
	| IMPLY(a,b) -> not (eval a) || (eval b)
	| LESS(a,b) -> if (value a) < (value b) then true else false


