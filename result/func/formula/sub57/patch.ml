type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval (f : formula) : bool = true

let rec eval (f : formula) : bool =
  let rec expin (f : exp) : int =
    match f with
    | Num n -> n
    | Plus (e1, e2) -> expin e1 + expin e2
    | Minus (e1, e2) -> expin e1 - expin e2
  in

  match f with
  | True -> true
  | False -> false
  | Not f1 -> if eval f1 then false else true
  | AndAlso (f1, f2) -> if eval f1 then eval f2 else false
  | OrElse (f1, f2) -> if f1 = True || f2 = True then true else eval f2
  | Imply (f1, f2) -> if eval f1 then eval f2 else true
  | Equal (e1, e2) -> if expin e1 = expin e2 then true else false
