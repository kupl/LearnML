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

let rec cal exp =
	match exp with
	| Num (e1) -> e1
	| Plus (e1, e2) -> let a = cal e1 + cal e2 in a
	| Minus (e1, e2) -> let a = cal e1 - cal e2 in a

let rec eval f =
	match f with
	| True -> true
 	| False -> false
	| Not (f1) -> if eval f1 then false else true
	| AndAlso (f1, f2) -> if eval f1 && eval f2 then true else false 
	| OrElse (f1, f2) -> if eval f1 || eval f2 then true else false
	| Imply (f1, f2) -> if eval f1 && eval (Not (f2)) then false else true
	| Equal (n1, n2) -> if (cal n1) = (cal n2) then true else false

