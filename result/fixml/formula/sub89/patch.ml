type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec value : exp -> int =
 fun n ->
  match n with
  | Num h -> h
  | Plus (x, y) -> value x + value y
  | Minus (x, y) -> value x - value y


let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not h -> not (eval h)
  | AndAlso (x, y) -> eval x && eval y
  | OrElse (x, y) -> eval x || eval y
  | Imply (x, y) -> if eval x = true then eval y else true
  | Equal (x, y) -> if value x = value y then true else false
