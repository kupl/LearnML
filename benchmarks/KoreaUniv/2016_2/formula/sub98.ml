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
= fun f ->
	match f with
	| True -> true
	| False -> false
	| Not f1 -> if eval f1 then false else true
	| AndAlso (f1, f2) -> if (eval f1) && (eval f2) then true else false
	| OrElse (f1, f2) -> if (eval f1) || (eval f2) then true else false
	| Imply (f1, f2) -> if eval f1 then eval f2 else true
	| Equal (e1, e2) -> let rec cal n = match n with
									| Num n1 -> n1
									| Plus (n1, n2) -> (cal n1) + (cal n2)
									| Minus (n1, n2) -> (cal n1) - (cal n2)
								in if (cal e1) = (cal e2) then true else false