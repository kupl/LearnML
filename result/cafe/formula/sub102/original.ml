type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec js (f : exp) : int =
  match f with
  | Num n -> n
  | Plus (a1, a2) -> js a1 + js a2
  | Minus (a1, a2) -> js a1 - js a2


let eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not a -> if a = True then false else true
  | AndAlso (a1, a2) -> if a1 = True && a2 = True then true else false
  | OrElse (a1, a2) -> if a1 = True || a2 = True then true else false
  | Imply (a1, a2) -> if a1 = True && a2 = False then false else true
  | Equal (a1, a2) -> if js a1 = js a2 then true else false


let (_ : bool) = eval (Imply (Imply (True, False), True))

let (_ : bool) = eval (Equal (Num 1, Plus (Num 1, Num 2)))

let (_ : bool) = eval (Equal (Num 3, Plus (Num 1, Num 2)))
