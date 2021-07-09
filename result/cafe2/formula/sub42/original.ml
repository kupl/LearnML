type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec exptoint (e : exp) : int =
  match e with
  | Num a -> a
  | Plus (a, b) -> exptoint a + exptoint b
  | Minus (a, b) -> exptoint a - exptoint b


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not f1 -> if f1 = True then false else true
  | AndAlso (f1, f2) -> if f1 = True && f2 = True then true else false
  | OrElse (f1, f2) -> if AndAlso (f1, f2) = False then false else true
  | Imply (f1, f2) ->
      if Not f1 = True || AndAlso (f1, f2) = True then true else false
  | Equal (e1, e2) -> if exptoint e1 = exptoint e2 then true else false
