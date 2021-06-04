type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval f =
  let rec eval_exp e =
    match e with
    | Num n -> n
    | Plus (a, b) -> eval_exp a + eval_exp b
    | Minus (a, b) -> eval_exp a - eval_exp b
  in

  match f with
  | True -> true
  | False -> false
  | Not a -> eval a
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b) -> eval a || eval b
  | Imply (a, b) -> not (eval a && not (eval b))
  | Equal (a, b) -> eval_exp a = eval_exp b
