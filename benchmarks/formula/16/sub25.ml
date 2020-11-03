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
= fun e ->
        match e with
        | Num n -> n
        | Plus (e1, e2) -> (exp_to_int e1)  + (exp_to_int e2) 
        | Minus (e1, e2) -> (exp_to_int e1) - (exp_to_int e2);;

let rec eval : formula -> bool
= fun f ->
        match f with
        | True -> true
        | False -> false
        | Not f1 -> if (eval f1) then false else true
        | AndAlso (f1, f2) -> if (eval f1) then (eval f2) else false
        | OrElse (f1, f2) -> if (eval f1) then true else (eval f2)
        | Imply (f1, f2) -> eval (OrElse (Not f1, f2))
        | Equal (e1, e2) -> (exp_to_int e1) == (exp_to_int e2);;
