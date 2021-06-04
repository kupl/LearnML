type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec chlwhd (e : exp) : int =
  match e with
  | Num a -> a
  | Plus (a, b) -> chlwhd a + chlwhd b
  | Minus (a, b) -> chlwhd a - chlwhd b


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not f -> not (eval f)
  | AndAlso (f', f) -> eval f' && eval f
  | OrElse (f', f) -> eval f' || eval f
  | Imply (f', f) -> if eval f' = true && eval f = false then false else true
  | Equal (a, b) -> if chlwhd a = chlwhd b then true else false
