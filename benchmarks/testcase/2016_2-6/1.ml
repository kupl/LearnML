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

let rec f : formula -> bool
= fun f -> (* TODO *)
match f with 
| True -> true
| False -> false
| Not a -> if f(a)=true then false else true
| AndAlso (a,b) -> if f(a) && f(b) then true else false
| OrElse (a,b) -> if (f(a) = false) && (f(b) = false) then false else true
| Imply (a,b) -> if (f(a) && f(b)) = false then false else true
| Equal (a,b) -> if(a=b) then true else false
;;