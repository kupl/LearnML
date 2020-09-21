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

let rec eval2 : exp -> int
= fun e ->
match e with
| Num (e1) -> e1
| Plus (e1,e2) -> (eval2 e1) + (eval2 e2)
| Minus (e1,e2) -> (eval2 e1) - (eval2 e2)

let rec eval : formula -> bool
= fun f -> 
match f with
| True -> true
| False -> false
| Not (formula) -> (if (eval formula) = true then false else true)
| AndAlso (formula1, formula2) -> (eval formula1 && eval formula2)
| OrElse (formula1, formula2) -> (eval formula1 || eval formula2)
| Imply (formula1, formula2) -> 
(if ((eval formula1)=true && (eval formula2)=false) then false else true)
| Equal (exp1, exp2) -> (if (eval2 exp1) = (eval2 exp2) then true else false);;
