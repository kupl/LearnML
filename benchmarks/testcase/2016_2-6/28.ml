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
    let rec value : exp -> int = fun x -> match x with
        | Num (a)      -> a
        | Plus (a, b)  -> (value a) + (value b)
        | Minus (a, b) -> (value a) * (value b)
    in match form with
    | True  -> true
    | False -> false
    | Not (x) -> not (f x)
    | AndAlso (x, y) -> (f x) && (f y)
    | OrElse (x, y) -> (f x) || (f y)
    | Imply (x, y) -> (not (f x)) || (f y)
    | Equal (x, y) -> (value x) = (value y)
;;

