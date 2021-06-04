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
  match f with
  | True -> true
  | False -> false
  | Not f -> (
      match f with
      | True -> false
      | False -> true
      | AndAlso (a, b) -> if eval a = eval b then true else false
      | OrElse (a, b) -> if eval a = eval b then false else true
      | Imply (a, b) -> if b = True then true else false
      | Equal (a, b) -> if a = b then true else false )


let (_ : bool) = eval (Not True)

let (_ : bool) = eval (Not False)
