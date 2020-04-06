type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 



let rec eval_op op =
 match op with
|Num a -> a
|Minus(exp1, exp2) -> (eval_op exp1) - (eval_op exp2)
|Plus(exp1, exp2) -> (eval_op exp1) + (eval_op exp2) 

and eval f =
match f with
|True -> true
|False -> false
|Not f -> not (eval f)
|AndAlso (f1,f2)-> (eval f1)&&(eval f2)
|OrElse (f1,f2)-> (eval f1)||(eval f2)
(*|Imply (f1,f2) -> (not(eval f1))&&(eval f2)*)
|Equal (exp1,exp2) ->  if ((eval_op exp1) = (eval_op exp2)) then true else false;;

