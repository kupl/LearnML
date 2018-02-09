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
  | Minus of exp * exp;; 

let rec evalExp : exp -> int
= fun x -> match x with
| Num a -> a
| Plus (a,b) -> (evalExp a) + (evalExp b)
| Minus (a,b) -> (evalExp a) - (evalExp b);;

let rec eval : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Not k -> if (eval k) then false else true
| AndAlso (a, b) -> if ( (eval a) && (eval b) ) then true else false
| OrElse (a, b) -> if ( (eval a) || (eval b) ) then true else false
| Imply (a, b) -> if ( (eval a) && (eval (Not b)) ) then false else true
| Equal (a, b) -> if ( (evalExp a) = (evalExp b) ) then true else false;;
