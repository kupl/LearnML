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

let rec eval : formula -> bool
= fun f ->
  let rec expEval exp =
    match exp with
    | Num i -> i
    | Plus (exp1,exp2) -> (expEval exp1) + (expEval exp2)
    | Minus (exp1,exp2) -> (expEval exp1) - (expEval exp2)
  in
    match f with
    | True -> true
    | False -> false
    | Not g -> not (eval g)
    | AndAlso (g,h) -> (eval g) && (eval h)
    | OrElse (g,h) -> (eval g) || (eval h)
    | Imply (g,h) -> (not (eval g)) || (eval h)
    | Equal (exp1, exp2) -> (expEval exp1) = (expEval exp2);;

