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
 fun f -> match f with True -> true | False -> false | _ -> eval f


let _ = eval (Imply (Imply (True, False), True))

let _ = eval (Equal (Num 1, Plus (Num 1, Num 2)))
