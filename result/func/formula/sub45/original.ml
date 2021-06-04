type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec evalexp (f : exp) : int =
  match f with
  | Num x -> x
  | Plus (x, y) -> evalexp x + evalexp y
  | Minus (x, y) -> evalexp x + evalexp y


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not p -> if eval p = true then false else true
  | AndAlso (p, q) -> if eval p = true && eval q = true then true else false
  | OrElse (p, q) -> if eval p = true || eval q = true then true else false
  | Imply (p, q) -> if eval q = true then true else false
  | Equal (p, q) -> if evalexp p = evalexp q then true else false
