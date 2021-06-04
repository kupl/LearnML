type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval_exp (e : exp) : int =
  match e with
  | Num x -> x
  | Plus (x, y) -> eval_exp x + eval_exp y
  | Minus (x, y) -> eval_exp x - eval_exp y


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not a -> if a = True then false else true
  | AndAlso (x, y) -> if x = True && y = True then true else false
  | OrElse (x, y) -> if x = False && y = False then false else true
  | Imply (x, y) ->
      if x = False && y = False then true
      else if x = False && y = True then true
      else if x = True && y = False then false
      else true
  | Equal (x, y) -> if eval_exp x = eval_exp y then true else false
