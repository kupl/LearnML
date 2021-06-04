type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval e =
  let rec expEval exp =
    match exp with
    | Num i -> i
    | Plus (e1, e2) -> expEval e1 + expEval e2
    | Minus (e1, e2) -> expEval e1 - expEval e2
  in

  match e with
  | True -> true
  | False -> false
  | Not f -> not (eval f)
  | AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Imply (f1, f2) -> eval (OrElse (f2, Not f1))
  | Equal (e1, e2) -> expEval e1 = expEval e2
