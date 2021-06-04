type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec check exp =
  match exp with
  | Num a -> a
  | Plus (a, b) -> check exp + check exp
  | Minus (a, b) -> check exp - check exp


let rec eval form =
  match form with
  | True -> true
  | False -> false
  | Not a -> eval a
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b) -> eval a || eval b
  | Imply (a, b) -> eval (OrElse (Not a, b))
  | Equal (a, b) -> check a = check b
