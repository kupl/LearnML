type exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp




let rec eval_op op =
 match op with
|Num a -> a
|Minus(exp1, exp2) -> (eval_op exp1) - (eval_op exp2)
|Plus(exp1, exp2) -> (eval_op exp1) + (eval_op exp2) ;;

and f form =
match form with
|True -> true
|False -> false
|Not f1 -> not (f f1)
|AndAlso (f1,f2)-> (f f1)&&(f f2)
|OrElse (f1,f2)-> (f f1)||(f f2)
|Imply (f1,f2) -> (not(f f1))&&(f f2)
|Equal (exp1,exp2) ->  if ((eval_op exp1) = (eval_op exp2)) then true else false;;

