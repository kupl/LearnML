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
= fun exp ->
  match exp with
  | Num n -> n
  | Plus (e1, e2) -> (exp_eval e1) + (exp_eval e2)
  | Minus (e1, e2) -> (exp_eval e1) - (exp_eval e2)

let rec eval : formula -> bool
= fun f ->
  match f with
  | True -> true
  | False -> false
  | Not f -> not (eval f)
  | AndAlso (f1, f2) -> (eval f1) && (eval f2)
  | OrElse (f1, f2) -> (eval f1) || (eval f2)
  | Imply (f1, f2) -> (not (eval f1)) || (eval f2)
  | Equal (e1, e2) -> (exp_eval e1) = (exp_eval e2)