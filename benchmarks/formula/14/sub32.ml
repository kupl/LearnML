exception TODO

type formula = 
 True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp

and exp = 	
	Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec value (e: exp) : int =
	match e with
	| Num t -> t
	| Plus (t,z) -> (value t) + (value z)
	| Minus (t,z) -> (value t) - (value z)

let rec eval (f: formula) : bool =
	match f with
	| True -> true 
	| False -> false
	| AndAlso (x,y) -> (eval x) && (eval y)
	| OrElse (x,y) -> (eval x) || (eval y)
	| Imply (x,y) -> not (eval x) || (eval y)
	| Equal (x,y) -> (value x) = (value y)
	| _ -> raise TODO
