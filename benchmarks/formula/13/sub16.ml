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


let rec value v =
                match v with
                | NUM x -> x
                | PLUS (x, y) -> (value x) + (value y)
                | MINUS (x, y) -> (value x) - (value y)

let rec eval a = 
	match a with
	| TRUE -> true
	| FALSE -> false
	| NOT b -> not (eval b)
	| ANDALSO(b, c) -> (eval b) && (eval c)
	| ORELSE(b, c) -> (eval b) || (eval c)
	| IMPLY(b, c) -> (not (eval b)) || (eval c)
	| LESS(b, c) -> (value b) < (value c)
