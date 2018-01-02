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

let rec arithmetic = fun f -> match f with 
| Num num -> num
| Plus(num1,num2) -> (arithmetic num1) + (arithmetic num2) 
| Minus(num1,num2) -> (arithmetic num1) - (arithmetic num2)
;;
let rec f : formula -> bool
= fun form -> match form with
| True -> true
| False -> false
| Not fm -> if f fm then false else true
| AndAlso(fm1,fm2) -> (f fm1) && (f fm2)
| OrElse(fm1,fm2) -> (f fm1) || (f fm2)
| Imply(fm1,fm2) -> if (f fm1) then if (f fm2) then true else false else false 
| Equal(exp1,exp2) -> (arithmetic exp1) = (arithmetic exp2) 
;;
