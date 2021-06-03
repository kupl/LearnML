(* 2006-11782 Song Young-chan, Hw2-3 True/False *)

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

let rec eval_exp expession = 
	match expession with
	  Num(number) -> number
	| Plus(num1, num2) -> ((eval_exp num1) + (eval_exp num2))
	| Minus(num1, num2) -> ((eval_exp num1) - (eval_exp num2))

let rec eval input_formula = 
	match input_formula with
	  True -> true
	| False -> false
	| Not(t_f) -> if ((eval t_f)=true) then false
		      else true
	| AndAlso(t_f1, t_f2) -> ((eval t_f1) && (eval t_f2))
	| OrElse(t_f1, t_f2) -> ((eval t_f1) || (eval t_f2))
	| Imply(t_f1, t_f2) -> if (((eval t_f1) = false) || ((eval t_f2) = true)) then true
			       else false
	| Equal(exp1, exp2) -> if ((eval_exp exp1) = (eval_exp exp2)) then true
				else false
