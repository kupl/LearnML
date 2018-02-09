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

let rec eval : formula -> bool =
let rec expfunc =
fun e -> match e with
Num k -> k|
Plus (k, l) -> (expfunc k) + (expfunc l)|
Minus (k, l) -> (expfunc k) - (expfunc l)

in fun f -> match f with
Not k -> if (eval k) == true then false else true |
AndAlso (k, l) -> if(eval k == true  && eval l == true) then true else false| 
OrElse (k, l) -> if(eval k == true || eval l == true) then true else false|
Imply (k, l) -> if(eval k == true && eval l == false) then false else true|
Equal (k, l) -> if (expfunc k == expfunc l) then true else false|
True -> true | False -> false
