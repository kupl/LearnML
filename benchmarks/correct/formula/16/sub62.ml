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

let rec exp_eval : exp -> int
= fun e -> match e with
    Num a       -> a
  | Plus (a, b) -> exp_eval a + exp_eval b
  | Minus(a, b) -> exp_eval a - exp_eval b;;  

let rec eval : formula -> bool
= fun f ->  match f with
    True  -> true
  | False -> false 
  | Not a -> if eval a then false else true
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b)  -> eval a || eval b
  | Imply (a, b)   ->
      if eval a = true && eval b = false then false
      else true
  | Equal (a, b)   ->
      if exp_eval a = exp_eval b then true
      else false;; 
