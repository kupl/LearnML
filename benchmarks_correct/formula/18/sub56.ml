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
  let rec eval_exp : exp -> int
  = fun num ->
    match num with 
      Num n -> n
      | Plus (n1, n2) -> eval_exp(n1) + eval_exp(n2)
      | Minus (n1, n2) -> eval_exp(n1) - eval_exp(n2)
  in match f with
  True -> true
  | False -> false
  | Not p -> not (eval p)
  | AndAlso (p, q) -> eval p && eval q
  | OrElse (p, q) -> eval p || eval q
  | Imply (p, q) -> not (eval p) || eval q
  | Equal (n1, n2) -> eval_exp(n1) = eval_exp(n2);;
