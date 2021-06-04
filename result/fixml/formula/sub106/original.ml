type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval_exp : exp -> int =
 fun e ->
  match e with
  | Num n -> n
  | Plus (e1, e2) -> eval_exp e1 + eval_exp e2
  | Minus (e1, e2) -> eval_exp e1 - eval_exp e2


let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not b -> not (eval b)
  | AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Imply (f1, f2) -> if eval f1 = false && eval f2 then false else true
  | Equal (e1, e2) -> eval_exp e1 = eval_exp e2
