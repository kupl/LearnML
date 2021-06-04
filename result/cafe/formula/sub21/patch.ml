type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval_exp (exp : exp) : int =
  match exp with
  | Num e -> e
  | Plus (e1, e2) -> eval_exp e1 + eval_exp e2
  | Minus (e1, e2) -> eval_exp e1 - eval_exp e2


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not f1 -> if eval f1 then false else true
  | AndAlso (f1, f2) -> if eval f1 && eval f2 then true else false
  | OrElse (f1, f2) -> if eval f1 || eval f2 then true else false
  | Imply (f1, f2) -> if eval f1 = false || eval f2 then true else false
  | Equal (e1, e2) -> if eval_exp e1 = eval_exp e2 then true else false
