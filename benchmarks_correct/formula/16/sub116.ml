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

let rec exp_to_int : exp -> int
= fun ex -> match ex with
| Num (n) -> n
| Plus (n1, n2) -> exp_to_int(n1) + exp_to_int(n2)
| Minus (n1, n2) -> exp_to_int(n1) - exp_to_int(n2)

let rec eval : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Not (a) -> not (eval a)
| AndAlso (a, b) -> if (eval a) then (eval b) else false
| OrElse (a, b) -> if (eval a) then true else (eval b)
| Imply (a, b) -> if (eval a) then (eval b) else true
| Equal (a, b) -> if exp_to_int(a) = exp_to_int(b) then true else false
