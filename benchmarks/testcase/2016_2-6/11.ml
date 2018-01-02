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
= fun form -> 
match form with
True -> true
|False -> false
|Not a -> if a = True then false else true
|AndAlso (a, b)-> if a = True && b = True then true else false
|OrElse (a, b) -> if a = True || b = True then true else false
|Imply (a, b) -> if (Not a) = True ||  b = True then true else false
|Equal (a, b) -> let rec evalexp e 
= match e with
Num a -> a
|Plus (a, b) -> evalexp a + evalexp b
|Minus (a, b) -> evalexp a - evalexp b
in if evalexp a = evalexp b then true else false;;
 (* TODO *)

