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
= fun f -> (* TODO *)
match f with 
|	True -> true
| False -> false
| Not a when eval(a)=true -> false
| Not a -> true
| AndAlso (a,b) when eval(a) && eval(b) -> true
| AndAlso (a,b) -> false
| OrElse (a,b) when eval(a) = false && eval(b) = false -> false
| OrElse (a,b) -> true
| Imply (a,b) when eval(a) && eval(b)= false -> false
| Imply (a,b) -> true
| Equal (a,b) when a=b -> true
| Equal (a,b) ->false
