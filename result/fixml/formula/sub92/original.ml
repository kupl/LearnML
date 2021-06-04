type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eq : exp -> int =
 fun f ->
  match f with
  | Num a -> a
  | Plus (a, b) -> eq (Num (eq a + eq b))
  | Minus (a, b) -> eq (Num (eq a - eq b))


let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not True -> false
  | Not False -> true
  | Not a -> eval a
  | Equal (a, b) -> if eq a = eq b then true else false
  | Imply (a, b) -> (
      match (a, b) with
      | True, True -> true
      | True, False -> false
      | False, True -> true
      | False, False -> true
      | _, _ ->
          eval
            (Imply
               ( if eval a = true then True
               else (False, if eval b = true then True else False) )) )
  | AndAlso (a, b) -> (
      match (a, b) with
      | True, True -> true
      | True, False -> false
      | False, True -> false
      | False, False -> false
      | _, _ ->
          eval
            (AndAlso
               ( if eval a = true then True
               else (False, if eval b = true then True else False) )) )
  | OrElse (a, b) -> (
      match (a, b) with
      | True, True -> true
      | True, False -> true
      | False, True -> true
      | False, False -> false
      | _, _ ->
          eval
            (OrElse
               ( if eval a = true then True
               else (False, if eval b = true then True else False) )) )
