type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval formula =
  let rec eval_exp e =
    match e with
    | Num x -> x
    | Plus (x, y) -> eval_exp x + eval_exp y
    | Minus (x, y) -> eval_exp x + eval_exp y
  in

  match formula with
  | True -> true
  | False -> false
  | Not x -> not (eval x)
  | AndAlso (x, y) -> eval x && eval y
  | OrElse (x, y) -> eval x || eval y
  | Imply (x, y) -> (not (eval x)) || eval y
  | Equal (x, y) -> eval_exp x = eval_exp y
