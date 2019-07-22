(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-1.ml *)

type formula = TRUE
	     | FALSE
	     | NOT of formula
	     | ANDALSO of formula * formula
	     | ORELSE of formula * formula
	     | IMPLY of formula * formula
	     | LESS of expr * expr
and expr = NUM of int
	 | PLUS of expr * expr
	 | MINUS of expr * expr

let rec e2i (e : expr) : int =
  match e with
  | NUM n -> n
  | PLUS (m, n) -> (e2i m) + (e2i n)
  | MINUS (m, n) -> (e2i m) - (e2i n)

let rec eval (f : formula) : bool =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT p -> not (eval p)
  | ANDALSO (p, q) -> (eval p) && (eval q)
  | ORELSE (p, q) -> (eval p) || (eval q)
  | IMPLY (p, q) ->
      if ((eval p = true) && (eval q = false)) then false
      else true
  | LESS (m, n) -> (e2i m) < (e2i n)
