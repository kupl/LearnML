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
= fun f -> true (* TODO *)
(*	match f with
	| True -> true
	|False->false
	|Not(x)->if x then false else true
	|AndAlso(x1,x2)-> x1 && x2
	|OrElse(x1,x2)-> x1||x2
	|Imply(x1,x2)->true 
	|Equal(x1,x2)->if x1=x2 then true else false
*)
