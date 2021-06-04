type exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

let rec eval_exp a =
  match a with
  | Num n -> n
  | Plus (n1, n2) -> eval_exp n1 + eval_exp n2
  | Minus (n1, n2) -> eval_exp n1 + eval_exp n2


let rec eval thing =
  match thing with
  | True -> true
  | False -> false
  | Not a -> if eval a then false else true
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b) -> eval a || eval b
  | Imply (a, b) -> if eval a = true && eval b = false then false else true
  | Equal (a, b) -> if eval_exp a = eval_exp b then true else false
