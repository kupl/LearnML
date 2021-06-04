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
  | Not f1 -> if eval f1 then false else true
  | AndAlso (f1, f2) -> if eval f1 then eval f2 else false
  | OrElse (f1, f2) -> if f1 = True then true else eval f2
  | Imply (f1, f2) -> if eval f1 then eval f2 else true
  | Equal (a, b) -> if evalexp a = evalexp b then true else false
