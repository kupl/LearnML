(* Computer Science/2005-11759/Sangcheol Park/Exercise 2-3.*)
type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp;;

let rec eval f =
	let rec eval_exp e =
		match e with
		| Num n -> n
		| Plus(a, b) -> (eval_exp a + eval_exp b)
		| Minus(a, b) -> (eval_exp a - eval_exp b)
	in
	match f with
	| True -> true
	| False -> false
	| Not a -> eval a
	| AndAlso(a, b) -> (eval a) && (eval b)
	| OrElse(a, b) -> (eval a) || (eval b)
	| Imply(a, b) -> not  ((eval a) && not (eval b))
	| Equal(a, b) -> (eval_exp a) = (eval_exp b)
;;

(* 
eval True;;
eval False;;
eval(Not True);;
eval(Not False);;
eval(AndAlso(True, False));;
eval(OrElse(True, False));;
eval(Imply(True, False));;
eval(Imply(False, False));;
eval(Imply(False, True));;
eval(Imply(True, True));;
eval(AndAlso(Equal(Plus(Num 10, Num 20), Minus(Num 100, Plus(Num 30, Num 40))), True));;
*)