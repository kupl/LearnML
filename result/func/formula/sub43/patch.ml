type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec expfun (q : exp) : int =
  match q with
  | Num i -> i
  | Plus (i, k) -> expfun i + expfun k
  | Minus (i, k) -> expfun i - expfun k


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not f1 -> if eval f1 then false else true
  | AndAlso (f1, f2) -> if eval f1 then eval f2 else false
  | OrElse (f1, f2) -> if eval f1 then true else eval f2
  | Imply (f1, f2) -> if eval f1 then eval f2 else true
  | Equal (q1, q2) -> if expfun q1 = expfun q2 then true else false
