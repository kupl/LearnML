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

let rec eval_e: expr -> int = fun e ->
	match e with
	NUM (n) -> n
	| PLUS (n1, n2) -> (eval_e n1) + (eval_e n2)
	| MINUS (n1, n2) -> (eval_e n1) - (eval_e n2)

let rec eval: formula -> bool = fun f ->
	match f with
	TRUE -> true
	| FALSE -> false
	| NOT(n) -> not (eval n)
	| ANDALSO(n1, n2) -> (eval n1) && (eval n2)
	| ORELSE(n1, n2) -> (eval n1) || (eval n2)
	| IMPLY(n1, n2) -> not (eval n1) || ((eval n1) && (eval n2))
	| LESS(e1, e2)-> (eval_e (MINUS(e1, e2))) < 0
