type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec cal (exp : exp) : int =
  match exp with
  | Num x -> x
  | Plus (x, y) -> cal x + cal y
  | Minus (x, y) -> cal x - cal y


let rec eval (formula : formula) : bool =
  match formula with
  | True -> true
  | False -> false
  | Not x -> not (eval x)
  | AndAlso (x, y) -> eval x && eval y
  | OrElse (x, y) -> eval x || eval y
  | Imply (x, y) ->
      if eval x = false || eval y = true then true
      else if y != True || y != False then eval y
      else if x = False then true
      else if y = True then true
      else false
  | Equal (e1, e2) -> if cal e1 = cal e2 then true else false
