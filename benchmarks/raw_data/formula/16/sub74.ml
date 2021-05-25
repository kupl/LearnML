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
= fun f -> match f with
  | True -> true
  | False -> false
  | Not g -> if eval g then false else true
  | AndAlso (g, h) -> if eval g && eval h then true else false
  | OrElse (g, h) -> if eval g==false && eval h==false then false else true
  | Imply (g, h) -> if eval g && eval h == false then false else true
  | Equal (exp1, exp2) -> 
    let rec eval_exp e = match e with
      | Num i -> i
      | Plus (i1, i2) -> eval_exp i1 + eval_exp i2
      | Minus (i1, i2) -> eval_exp i1 - eval_exp i2 
    in if eval_exp exp1 == eval_exp exp2 then true else false
