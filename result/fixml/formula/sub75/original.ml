type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not a -> if a = True then false else true
  | OrElse (a, b) -> if a = True || b = True then true else false
  | AndAlso (a, b) -> if a = True && b = True then true else false
  | Imply (a, b) -> if a = True && b = False then false else true
  | Equal (a, b) -> if a = b then true else false
