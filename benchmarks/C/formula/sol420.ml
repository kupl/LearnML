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

let rec exee : exp -> int
= fun f ->
		match f with
		 | Num(f) ->f
		 | Plus (f1,f2) -> (exee f1) + (exee f2)
		 | Minus (f1,f2) -> (exee f1) - (exee f2)

let rec eval : formula -> bool
= fun f -> 
		match f with
		 | True -> true
		 | False -> false
		 | Not f -> not(eval f)
		 | AndAlso (n1, n2) -> if ((eval n1) = true && (eval n2) = true) then true else false
		 | OrElse (n1, n2) -> if ((eval n1) = false && (eval n2) = false) then false else true
		 | Imply (n1, n2) -> if ((eval n1) = true && (eval n2) = false) then false else true
		 | Equal (n1, n2) -> (exee n1) = (exee n2)


