type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec result (e : exp) : int =
  match e with
  | Num n -> n
  | Plus (n1, n2) -> result n1 + result n2
  | Minus (n1, n2) -> result n1 - result n2


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not f1 -> not (eval f1)
  | AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Imply (f1, f2) -> if eval f1 = false || eval f2 = true then true else false
  | Equal (e1, e2) -> result e1 = result e2
