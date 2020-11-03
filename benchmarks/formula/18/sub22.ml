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
  let rec exp f =
    match f with
      Num x -> x
      | Plus (x, y) -> (exp x) + (exp y)
      | Minus (x, y) -> (exp x) - (exp y)
      in match f with 
        True -> true
        | False -> false
        | Not (x) -> if (eval x) = true then false else true
        | AndAlso (x, y) -> if (eval x) = true || (eval y) = true then true else false
        | OrElse (x, y) -> if (eval x) = false && (eval y) = false then false else true
        | Imply (x, y) -> if (eval x) = true && (eval y) = false then false else true
        | Equal (x, y) -> if (exp x) = (exp y) then true else false ;;