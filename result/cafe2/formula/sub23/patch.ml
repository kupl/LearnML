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
  let rec eval2 (e : exp) : int =
    match e with
    | Num x -> x
    | Plus (x, y) -> eval2 x + eval2 y
    | Minus (x, y) -> eval2 x - eval2 y
  in

  match f with
  | True -> true
  | False -> false
  | Not x -> not (eval x)
  | AndAlso (x, y) -> eval x && eval y
  | OrElse (x, y) -> eval x || eval y
  | Imply (x, y) -> if eval x = false || eval y = true then true else false
  | Equal (x, y) -> if eval2 x = eval2 y then true else false
