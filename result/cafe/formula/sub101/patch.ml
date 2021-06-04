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
  | Not f -> if eval f = true then false else true
  | AndAlso (f1, f2) -> if eval f1 = false then false else eval f2
  | OrElse (f1, f2) -> if eval f1 = true then true else eval f2
  | Imply (f1, f2) -> if eval f1 = true && eval f2 = false then false else true
  | Equal (e1, e2) -> if __s3 e1 = __s3 e2 then true else false


let (_ : bool) = eval (Imply (Imply (True, False), True))

let (_ : bool) = eval (Equal (Num 1, Plus (Num 1, Num 2)))

let (_ : bool) = eval (Imply (Imply (False, True), False))
