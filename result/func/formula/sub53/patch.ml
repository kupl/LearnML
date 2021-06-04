type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec expfun (e : exp) : int =
  match e with
  | Num n -> n
  | Plus (e1, e2) -> expfun e1 + expfun e2
  | Minus (e1, e2) -> expfun e1 - expfun e2


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not f1 -> if eval f1 then false else true
  | AndAlso (f1, f2) -> if eval f1 then eval f2 else false
  | OrElse (f1, f2) -> if f1 = True || f2 = True then true else eval f2
  | Imply (f1, f2) -> if eval f1 then eval f2 else true
  | Equal (e1, e2) -> if expfun e1 = expfun e2 then true else false
