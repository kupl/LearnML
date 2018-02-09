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

let rec evalExp: exp -> int
= fun ex -> match ex with
        | Num(a) -> a
        | Plus(a, b) -> evalExp(a) + evalExp(b)
        | Minus(a, b) -> evalExp(a) - evalExp(b);;
let rec eval : formula -> bool
= fun f -> match f with
        | True -> true
        | False -> false
        | Not(a) -> not(eval a)
        | AndAlso(a, b) -> eval(a) && eval(b)
        | OrElse(a, b) -> eval(a) || eval(b)
        | Imply(a, b) -> let c = eval(a) in
                        let d = eval(b) in
                        if c && not d then false else true
        | Equal(a, b) -> if evalExp(a) = evalExp(b) then true else false;;
