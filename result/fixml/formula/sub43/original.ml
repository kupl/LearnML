type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec expfun : exp -> int =
 fun q ->
  match q with
  | Num i -> i
  | Plus (i, k) -> expfun i + expfun k
  | Minus (i, k) -> expfun i - expfun k


let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not f1 -> if f1 = True then false else true
  | AndAlso (f1, f2) -> if f1 = True && f2 = True then true else false
  | OrElse (f1, f2) -> if f1 = False && f2 = False then false else true
  | Imply (f1, f2) -> if f1 = True && f2 = False then false else true
  | Equal (q1, q2) -> if expfun q1 = expfun q2 then true else false
