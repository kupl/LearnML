(* Computer Science/2005-11759/Sangcheol Park/Exercise 2-3.*)
type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr;;

let rec eval f =
	let rec eval_expr e =
		match e with
		| NUM n -> n
		| PLUS(a, b) -> (eval_expr a + eval_expr b)
		| MINUS(a, b) -> (eval_expr a - eval_expr b)
	in
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT a -> eval a
	| ANDALSO(a, b) -> (eval a) && (eval b)
	| ORELSE(a, b) -> (eval a) || (eval b)
	| IMPLY(a, b) -> not  ((eval a) && not (eval b))
	| LESS(a, b) -> (eval_expr a) < (eval_expr b)
;;

(* 
eval TRUE;;
eval FALSE;;
eval(NOT TRUE);;
eval(NOT FALSE);;
eval(ANDALSO(TRUE, FALSE));;
eval(ORELSE(TRUE, FALSE));;
eval(IMPLY(TRUE, FALSE));;
eval(IMPLY(FALSE, FALSE));;
eval(IMPLY(FALSE, TRUE));;
eval(IMPLY(TRUE, TRUE));;
eval(ANDALSO(LESS(PLUS(NUM 10, NUM 20), MINUS(NUM 100, PLUS(NUM 30, NUM 40))), TRUE));;
*)