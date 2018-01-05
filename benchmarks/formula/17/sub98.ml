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

let rec e2i x = match x with
			|NUM a -> a
			|PLUS (a, b) -> (e2i a) + (e2i b)
			|MINUS (a, b) -> (e2i a) - (e2i b)

let rec eval x = match x with
			 |TRUE -> true
			 |FALSE -> false
			 |NOT a -> not (eval a)
			 |ANDALSO (a, b) -> (eval a) && (eval b)
			 |ORELSE (a, b) -> (eval a) || (eval b)
			 |IMPLY (a, b) -> (not (eval a)) || (eval b)
			 |LESS (a, b) -> (e2i a) < (e2i b)
