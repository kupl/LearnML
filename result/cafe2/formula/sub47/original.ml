type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not b -> if b = True then eval True else eval False
  | AndAlso (a, b) ->
      if a = False then eval False
      else if b = False then eval False
      else eval True
  | OrElse (a, b) ->
      if a = True then eval True else if b = True then eval True else eval False
  | Imply (a, b) ->
      if a = False then eval True
      else if b = True then eval True
      else eval False
  | Equal (a, b) -> if a = b then eval True else eval False
