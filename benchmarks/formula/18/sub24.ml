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

let eval : formula -> bool
= fun f -> 
let rec eval' e =
  match e with
    | Num _ -> 0
    | Plus (e1, e2) -> (eval' e1) + (eval' e2)
    | Minus (e1, e2) -> (eval' e1) - (eval' e2)
  in
  match f with
  | Equal (e1, e2) ->
      if (eval' e1) = (eval' e2)
      then true
      else false
  ;;

