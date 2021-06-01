type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec eval_e: exp -> int = fun e ->
	match e with
	Num (n) -> n
	| Plus (n1, n2) -> (eval_e n1) + (eval_e n2)
	| Minus (n1, n2) -> (eval_e n1) - (eval_e n2)

let rec eval: formula -> bool = fun f ->
	match f with
	True -> true
	| False -> false
	| Not(n) -> not (eval n)
	| AndAlso(n1, n2) -> (eval n1) && (eval n2)
	| OrElse(n1, n2) -> (eval n1) || (eval n2)
	| Imply(n1, n2) -> not (eval n1) || ((eval n1) && (eval n2))
	| Equal(e1, e2)-> (eval_e (Minus(e1, e2))) = 0
