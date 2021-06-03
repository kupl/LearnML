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

let rec exp2num e =
  match e with
  | Num n -> n
  | Plus (n1, n2) -> (exp2num n1) + (exp2num n2)
  | Minus (n1, n2) -> (exp2num n1) - (exp2num n2)

let rec eval f =
  match f with
  | True -> true
  | False -> false
  | Not p -> not (eval p)
  | AndAlso (p, q) -> (eval p) && (eval q)
  | OrElse (p, q) -> (eval p) || (eval q)
  | Imply (p, q) -> (not (eval p) || (eval q))
  | Equal (e1, e2) -> (exp2num e1) = (exp2num e2)
