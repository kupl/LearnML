type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec calc ex =
  match ex with
  | Num f -> f
  | Plus (a, b) -> calc a + calc b
  | Minus (a, b) -> calc a - calc b


let rec eval e =
  match e with
  | Not x -> fun x -> if x = True then false else true x
  | AndAlso (a, b) ->
      if a = False then false else if b = False then false else true
  | OrElse (a, b) -> if a = True then true else if b = True then true else false
  | Imply (a, b) -> if a = False && b = True then false else true
  | Equal (a, b) -> calc a = calc b
  | False -> false
  | True -> true
