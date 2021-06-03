(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-1.ml *)

type formula = True
	     | False
	     | Not of formula
	     | AndAlso of formula * formula
	     | OrElse of formula * formula
	     | Imply of formula * formula
	     | Equal of exp * exp
and exp = Num of int
	 | Plus of exp * exp
	 | Minus of exp * exp

let rec e2i (e : exp) : int =
  match e with
  | Num n -> n
  | Plus (m, n) -> (e2i m) + (e2i n)
  | Minus (m, n) -> (e2i m) - (e2i n)

let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not p -> not (eval p)
  | AndAlso (p, q) -> (eval p) && (eval q)
  | OrElse (p, q) -> (eval p) || (eval q)
  | Imply (p, q) ->
      if ((eval p = true) && (eval q = false)) then false
      else true
  | Equal (m, n) -> (e2i m) = (e2i n)
