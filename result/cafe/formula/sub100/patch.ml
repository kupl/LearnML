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
  | Not f -> not (eval f)
  | AndAlso (__s8, __s9) -> eval __s9 && eval __s8
  | Equal (__s5, __s6) -> if __s3 __s5 = __s3 __s6 then true else false
  | Imply (__s12, __s13) -> (not (eval __s12)) || eval __s13
  | OrElse (__s10, __s11) -> eval __s10 || eval __s11


let (_ : bool) = eval (Not True)

let (_ : bool) = eval (Not False)
