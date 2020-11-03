type formula =
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
and exp = Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec num : exp -> int = fun x ->
  match x with
  | Num n -> n
  | Plus (n1, n2) -> (num n1) + (num n2)
  | Minus (n1, n2) -> (num n1) - (num n2)

let rec eval : formula -> bool = fun x ->
  match x with
  | True -> true
  | False -> false
  | Not y -> not (eval y)
  | AndAlso (a, b) -> (eval a) && (eval b)
  | OrElse (a, b) -> (eval a) || (eval b)
  | Imply (a, b) -> (not (eval a)) || (eval b)
  | Equal (a, b) -> (num a) = (num b)
