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
  let rec calc : exp -> int
  = fun expr ->
    match expr with
      | Num x -> x
      | Plus (x,y) -> (calc x) + (calc y)
      | Minus (x,y) -> (calc x) - (calc y)
  in let rec evaluate : formula -> bool
    = fun form -> 
      match form with
        | True -> true
        | False -> false
        | Equal (x,y) -> if (calc x)=(calc y) then true else false
        | Not x -> not(evaluate x)
        | AndAlso (x,y) -> (evaluate x)&&(evaluate y)
        | OrElse (x,y) -> (evaluate x)||(evaluate y)
        | Imply (x,y) -> (not(evaluate x))||(evaluate y)
  in evaluate f;;

