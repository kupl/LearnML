exception Error of string;;

type formula = True
	       | False
	       | Not of formula
	       | AndAlso of formula * formula
	       | OrElse of formula * formula
	       | Imply of formula * formula
	       | Equal of exp * exp
and exp = Num of int
	   | Plus of exp * exp
	   | Minus of exp * exp;;

let rec calexp e = match e with 
    Num i -> i
  | Plus(e1, e2) -> (calexp e1) + (calexp e2)
  | Minus(e1, e2) -> (calexp e1) - (calexp e2);;

let rec eval f = match f with
    True -> true
  | False -> false
  | Not f1 -> if (eval f1) = true then false else true
  | AndAlso(f1, f2) -> (match ((eval f1), (eval f2)) with
			    (true, true) -> true
			  | (_, _) -> false)
  | OrElse(f1, f2) -> (match ((eval f1), (eval f2)) with
			    (false, false) -> false
			  | (_, _) -> true)
  | Imply(f1, f2) -> (match ((eval f1), (eval f2)) with
			  (true, false) -> false
			| (_, _) -> true)
  | Equal(e1, e2) -> if (calexp e1) = (calexp e2) then true else false;;
