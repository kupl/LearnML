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
  | Not a -> not (eval a)
  | AndAlso (x, y) -> eval x && eval y
  | OrElse (x, y) -> eval x || eval y
  | Imply (x, y) -> if eval x = false || eval y = true then true else false
  | Equal (x, y) -> if eval_exp x = eval_exp y then true else false
