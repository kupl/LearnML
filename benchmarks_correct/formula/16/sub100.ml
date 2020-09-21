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
  | Not a -> if eval a then false else true
  | AndAlso (a, b) -> if eval a then eval b else false
  | OrElse (a, b) -> if eval a then true else eval b
  | Imply (a, b) -> if eval a=false then true else eval b
  | Equal (a, b) -> let rec cal : exp -> int
                      = fun g ->
                        match g with
                        | Num a ->  a
                        | Plus (a, b) -> cal a + cal b
                        | Minus (a, b) -> cal a - cal b
                        in if cal a=cal b then true else false
