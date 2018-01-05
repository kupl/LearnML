type formula = TRUE | FALSE
			| NOT of formula
			| ANDALSO of formula * formula
			| ORELSE of formula * formula
			| IMPLY of formula * formula
			| LESS of expr * expr
and expr = NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr

let rec cal e =
	match e with
	|NUM x -> x
	|PLUS (a, b) -> cal a + cal b
	|MINUS (a, b) -> cal a - cal b

let rec eval f = 
	match f with
	|TRUE -> true
	|FALSE -> false
	|NOT a -> not(eval a)
	|ANDALSO (a, b) -> (eval a) && (eval b)
	|ORELSE (a, b) -> (eval a) || (eval b)
	|IMPLY (a, b) -> (not(eval a)) || (eval b)
	|LESS (a, b) -> (cal a) < (cal b)

