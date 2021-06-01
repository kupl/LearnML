type exp = Num of int
          | Plus of exp * exp
	  | Minus of exp * exp

type formula = True
             | False
	     | Not of formula
	     | AndAlso of formula * formula
	     | OrElse of formula * formula
	     | Imply of formula * formula
	     | Equal of exp * exp

let rec calculate e =
  match e with
  | Num n -> n
  | Plus (a, b) -> (calculate a) + (calculate b)
  | Minus (a, b) -> (calculate a) - (calculate b)

let rec eval f =
  match f with
  | True -> true
  | False -> false
  | Not a -> not (eval a)
  | AndAlso (a, b) -> (eval a) && (eval b)
  | OrElse (a, b) -> (eval a) || (eval b)
  | Imply (a, b) -> ((eval a) = false) || (eval (AndAlso (a, b)))
  | Equal (a, b) -> (calculate a) = (calculate b)
