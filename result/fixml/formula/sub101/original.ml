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
  match f with
  | True -> true
  | False -> false
  | Not f -> if eval f = true then false else true
  | AndAlso (f1, f2) -> if eval f1 = false then false else eval f2
  | OrElse (f1, f2) -> if eval f1 = true then true else eval f2
  | Imply (f1, f2) -> if eval f1 = true && eval f2 = false then false else true
  | Equal (e1, e2) -> if e1 = e2 then true else false


let _ = eval (Imply (Imply (True, False), True))

let _ = eval (Equal (Num 1, Plus (Num 1, Num 2)))

let _ = eval (Imply (Imply (False, True), False))
