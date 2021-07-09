type exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

let rec eval_exp (e : exp) : int =
  match e with
  | Num a -> a
  | Plus (a, b) -> eval_exp a + eval_exp b
  | Minus (a, b) -> eval_exp a - eval_exp b


let rec eval (a : formula) : bool =
  match a with
  | True -> true
  | False -> false
  | Not b -> if eval b = true then false else true
  | AndAlso (b, c) -> eval b && eval c
  | OrElse (b, c) -> eval b || eval c
  | Imply (b, c) -> (not (eval b)) || eval c
  | Equal (b, c) -> if eval_exp b = eval_exp c then true else false