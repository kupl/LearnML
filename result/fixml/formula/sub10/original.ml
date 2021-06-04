type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec cal exp =
  match exp with
  | Num x -> x
  | Plus (x, y) -> cal x + cal y
  | Minus (x, y) -> cal x - cal y


let rec eval formula =
  match formula with
  | True -> true
  | False -> false
  | Not x -> ( match x with True -> false | False -> true | _ -> eval x )
  | AndAlso (x, y) -> (
      if x != True || x != False then eval x
      else if y != True || y != False then eval y
      else match (x, y) with True, True -> true | _, _ -> false )
  | OrElse (x, y) -> (
      if x != True || x != False then eval x
      else if y != True || y != False then eval y
      else match (x, y) with False, False -> false | _, _ -> true )
  | Imply (x, y) ->
      if x != True || x != False then eval x
      else if y != True || y != False then eval y
      else if x = False then true
      else if y = True then true
      else false
  | Equal (e1, e2) -> if cal e1 = cal e2 then true else false
