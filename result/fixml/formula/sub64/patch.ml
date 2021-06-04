type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec evalHelper1 : exp -> int =
 fun f ->
  match f with
  | Num a -> a
  | Minus (a, b) -> evalHelper1 a - evalHelper1 b
  | Plus (a, b) -> evalHelper1 a + evalHelper1 b


let rec evalHelper : formula -> formula =
 fun f ->
  match f with
  | True -> True
  | False -> False
  | Not a -> if evalHelper a = True then False else True
  | AndAlso (a, b) ->
      if evalHelper a = True && evalHelper b = True then True else False
  | OrElse (a, b) ->
      if evalHelper a = False && evalHelper b = False then False else True
  | Imply (a, b) ->
      if evalHelper a = True && evalHelper b = False then False else True
  | Equal (a, b) -> if evalHelper1 a = evalHelper1 b then True else False


let rec eval : formula -> bool =
 fun f ->
  match f with True -> true | False -> false | f -> eval (evalHelper f)
