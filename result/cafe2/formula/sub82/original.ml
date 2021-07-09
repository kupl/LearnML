type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec value (a : exp) : int =
  match a with
  | Num a -> a
  | Plus (a, b) -> value a + value b
  | Minus (a, b) -> value a - value b


let eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not a -> if a = True then false else true
  | AndAlso (a, b) ->
      if a = False then false else if b = False then false else true
  | OrElse (a, b) -> if a = True then true else if b = True then true else false
  | Imply (a, b) -> if a = False then true else if b = True then true else false
  | Equal (a, b) -> if value a = value b then true else false
