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


let rec eeval f =
	match f with
	|	NUM a -> a
	| PLUS (a, b) -> (eeval a) + (eeval b)
	| MINUS (a, b) -> (eeval a) - (eeval b)


let rec eval f =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT a -> not (eval a) 
	| ANDALSO (a, b) -> (eval a) && (eval b) 
	| ORELSE (a, b) -> (eval a) || (eval b)
	| IMPLY (a, b) -> if (eval a)=false then true
										else (eval b)
	| LESS (a, b) -> (eeval a) < (eeval b)