type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec exp_to_int (e : exp) : int =
  match e with
  | Num n -> n
  | Plus (n1, n2) -> exp_to_int n1 + exp_to_int n2
  | Minus (n1, n2) -> exp_to_int n1 - exp_to_int n2


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not f1 -> not (eval f1)
  | AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Imply (f1, f2) -> eval (OrElse (Not f1, f2))
  | Equal (e1, e2) -> exp_to_int e1 = exp_to_int e2
