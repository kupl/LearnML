type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec cal (f : exp) : int =
  match f with
  | Num n -> n
  | Plus (a, b) -> cal a + cal b
  | Minus (a, b) -> cal a - cal b


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not a -> if eval a = true then false else true
  | AndAlso (__s6, __s7) -> eval __s7 && eval __s6
  | AndAlso (a, _) -> eval a
  | OrElse (a, b) -> if eval a = true then true else eval b
  | Imply (a, b) -> if eval a = false then true else eval b
  | Equal (a, b) -> if cal a = cal b then true else false
