exception Error of string;;

type formula = TRUE
	       | FALSE
	       | NOT of formula
	       | ANDALSO of formula * formula
	       | ORELSE of formula * formula
	       | IMPLY of formula * formula
	       | LESS of expr * expr
and expr = NUM of int
	   | PLUS of expr * expr
	   | MINUS of expr * expr;;

let rec calexp e = match e with 
    NUM i -> i
  | PLUS(e1, e2) -> (calexp e1) + (calexp e2)
  | MINUS(e1, e2) -> (calexp e1) - (calexp e2);;

let rec eval f = match f with
    TRUE -> true
  | FALSE -> false
  | NOT f1 -> if (eval f1) = true then false else true
  | ANDALSO(f1, f2) -> (match ((eval f1), (eval f2)) with
			    (true, true) -> true
			  | (_, _) -> false)
  | ORELSE(f1, f2) -> (match ((eval f1), (eval f2)) with
			    (false, false) -> false
			  | (_, _) -> true)
  | IMPLY(f1, f2) -> (match ((eval f1), (eval f2)) with
			  (true, false) -> false
			| (_, _) -> true)
  | LESS(e1, e2) -> if (calexp e1) < (calexp e2) then true else false;;
