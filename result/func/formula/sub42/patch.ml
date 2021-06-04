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
  | Not f1 -> if eval f1 then false else true
  | AndAlso (f1, f2) -> if eval f1 then eval f2 else false
  | OrElse (f1, f2) -> if eval f1 then true else eval f2
  | Imply (f1, f2) -> eval (OrElse (Not f1, f2))
  | Equal (e1, e2) -> if exptoint e1 = exptoint e2 then true else false
