type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec __s3 (__s4 : exp) : int =
  match __s4 with
  | Num __s14 -> __s14
  | Plus (__s15, __s16) -> __s3 __s15 + __s3 __s16
  | Minus (__s17, __s18) -> __s3 __s17 - __s3 __s18


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not p -> not (eval p)
  | AndAlso (p, q) -> eval p && eval q
  | OrElse (p, q) -> eval p || eval q
  | Imply (p, q) -> (not (eval p)) || eval q
  | Equal (p, q) -> if __s3 p = __s3 q then true else false
