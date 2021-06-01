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

let rec eval_exp exp_arg =
 match exp_arg with
 (Num n) -> n
 |(Plus (exp1, exp2)) -> ((eval_exp exp1) + (eval_exp exp2))
 |(Minus (exp1, exp2)) -> ((eval_exp exp1) - (eval_exp exp2))

let rec eval fmula =
 match fmula with
 True -> true
 |False -> false
 |(Not f1) -> (not (eval f1))
 |(AndAlso (f1, f2)) -> ((eval f1) && (eval f2))
 |(OrElse (f1, f2)) -> ((eval f1) || (eval f2))
 |(Imply (f1, f2)) -> (if (eval f1) then (eval f2) else true)
 |(Equal (exp1, exp2)) -> ((eval_exp exp1) = (eval_exp exp2))


