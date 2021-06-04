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
  | Not a -> if eval a then false else true
  | OrElse (a, b) -> if a = True || b = True then true else eval b
  | AndAlso (a, b) -> if eval a then eval b else false
  | Imply (a, b) -> if eval a then eval b else true
  | Equal (a, b) -> __s3 a = __s3 b
