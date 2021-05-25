exception TODO

type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
and exp =
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval1 (f1: exp): int =
  match f1 with
  | Num n -> n
  | Plus (a, b) -> (eval1 a) + (eval1 b)
  | Minus (a, b) -> (eval1 a) - (eval1 b)

let rec eval (f: formula): bool =
  match f with
  | True -> true
  | False -> false
  | Not n -> not (eval n)
  | AndAlso (a, b) -> (eval a) && (eval b)
  | OrElse (a, b) -> (eval a) || (eval b)
  | Imply (a, b) -> if (eval a) && (eval b) then true
		    else if (eval a) && not (eval b) then false
		    else if not (eval a) && (eval b) then true
		    else true
  | Equal (a, b) -> (eval1 a) = (eval1 b)
 
