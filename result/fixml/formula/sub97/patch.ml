type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval : formula -> bool =
 fun f ->
  let rec op : exp -> int =
   fun e ->
    match e with
    | Num i -> i
    | Plus (e1, e2) -> op e1 + op e2
    | Minus (e1, e2) -> op e1 - op e2
  in

  match f with
  | True -> true
  | False -> false
  | Not f -> eval f = false
  | AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Imply (f1, f2) -> (eval f1 && eval f2) || eval f1 = false
  | Equal (e1, e2) -> op e1 = op e2


let _ = eval (Imply (Imply (True, False), True))

let _ = eval (Equal (Num 1, Plus (Num 1, Num 2)))
