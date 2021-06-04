type exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

let rec eval (form : formula) : bool =
  let rec value (x : exp) : int =
    match x with
    | Num a -> a
    | Plus (a, b) -> value a + value b
    | Minus (a, b) -> value a - value b
  in

  match form with
  | True -> true
  | False -> false
  | Not x -> not (eval x)
  | AndAlso (x, y) -> eval x && eval y
  | OrElse (x, y) -> eval x || eval y
  | Imply (x, y) -> (not (eval x)) || eval y
  | Equal (x, y) -> value x = value y
