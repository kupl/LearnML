type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and
expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec eval_exp exp = match exp with
	| NUM a -> a
	| PLUS(a, b) -> (eval_exp a) + (eval_exp b)
	| MINUS(a, b) -> (eval_exp a) - (eval_exp b)

let rec eval formula = match formula with
	| TRUE -> true
	| FALSE -> false
	| NOT a -> not (eval a)
	| ANDALSO(a, b) -> (eval a) && (eval b)
	| ORELSE(a, b) -> (eval a) || (eval b)
	| IMPLY(a, b) -> (not (eval a)) || (eval b)
	| LESS(a, b) -> (eval_exp a) < (eval_exp b)