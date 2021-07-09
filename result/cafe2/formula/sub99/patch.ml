type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval_exp (f : exp) : int =
  match f with
  | Num a -> a
  | Plus (a, b) -> eval_exp a + eval_exp b
  | Minus (a, b) -> eval_exp a - eval_exp b


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not a -> not (eval a)
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b) -> eval a || eval b
  | Imply (a, b) -> eval (Not a) || eval b
  | Equal (a, b) -> if eval_exp a = eval_exp b then true else false


let (_ : bool) = eval True

let (_ : bool) = eval (Equal (Num 4, Plus (Num 1, Num 2)))
