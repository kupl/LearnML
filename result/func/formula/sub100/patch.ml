type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec __s1 (__s2 : exp) : int =
  match __s2 with
  | Num __s14 -> __s14
  | Plus (__s15, __s16) -> __s1 __s15 + __s1 __s16
  | Minus (__s17, __s18) -> __s1 __s17 - __s1 __s18


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not f -> if eval f then false else true
  | AndAlso (__s6, __s7) -> if eval __s7 && eval __s6 then true else false
  | Equal (__s12, __s13) -> if __s1 __s12 = __s1 __s13 then true else false
  | Imply (__s10, __s11) -> if eval __s10 = false then true else eval __s11
  | OrElse (__s8, __s9) -> if eval __s8 || eval __s9 then true else false


let (_ : bool) = eval (Not True)

let (_ : bool) = eval (Not False)
