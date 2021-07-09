type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let eval (fn : formula) : bool =
  match fn with
  | True -> true
  | False -> false
  | Not something -> ( match something with True -> false | False -> true )
  | AndAlso (True, True) -> true
  | AndAlso (True, False) -> false
  | AndAlso (False, _) -> false
  | OrElse (False, False) -> false
  | OrElse (False, True) -> true
  | OrElse (True, _) -> true
  | Imply (True, False) -> false
  | Imply (True, True) -> true
  | Imply (False, _) -> true
  | Equal (Num n1, Num n2) -> n1 = n2
  | Equal (Plus (Num n1, Num n2), Plus (Num n3, Num n4)) -> n1 + n2 = n3 + n4
  | Equal (Plus (Num n1, Num n2), Minus (Num n3, Num n4)) -> n1 + n2 = n3 - n4
  | Equal (Minus (Num n1, Num n2), Plus (Num n3, Num n4)) -> n1 - n2 = n3 + n4
  | Equal (Minus (Num n1, Num n2), Minus (Num n3, Num n4)) -> n1 - n2 = n3 - n4
