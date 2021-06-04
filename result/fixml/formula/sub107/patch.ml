type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec cal : exp -> int =
 fun e ->
  match e with
  | Num a -> a
  | Plus (a, b) -> cal a + cal b
  | Minus (a, b) -> cal a - cal b


let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not temp -> not (eval temp)
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b) -> eval a || eval b
  | Imply (a, b) -> if eval a = true && eval b = false then false else true
  | Equal (a, b) -> if cal a = cal b then true else false


let _ = eval (Equal (Num 1, Plus (Num 1, Num 2)))

let _ = eval (Imply (Imply (True, False), True))
