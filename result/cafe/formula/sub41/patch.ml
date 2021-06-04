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
  let rec evalexp (f1 : exp) : int =
    match f1 with
    | Num a -> a
    | Plus (a, b) -> evalexp a + evalexp b
    | Minus (a, b) -> evalexp a - evalexp b
  in

  match f with
  | True -> true
  | False -> false
  | Not f1 -> not (eval f1)
  | AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Imply (f1, f2) -> if eval f1 = false || eval f2 = true then true else false
  | Equal (a, b) -> if evalexp a = evalexp b then true else false
