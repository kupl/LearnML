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

let rec eval : formula -> bool
= fun f -> let rec calcExp e = (match e with
	Num(a) -> a
	|Plus(sub1,sub2) -> (calcExp sub1) + (calcExp sub2)
	|Minus(sub1,sub2) -> (calcExp sub1) - (calcExp sub2)) in (match f with
	True -> true
	|False -> false
	|Not(sub_f) -> not (eval sub_f)
	|AndAlso(f1,f2) -> (eval f1) && (eval f2)
	|OrElse(f1,f2) -> (eval f1) || (eval f2)
	|Imply(f1,f2) -> not ((eval f1) && (not (eval f2)))
	|Equal(e1,e2) -> (calcExp e1) = (calcExp e2)) (* TODO *)

