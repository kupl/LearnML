type expr = NUM of int
		  | PLUS of expr * expr
		  | MINUS of expr * expr

type formula = TRUE
			 | FALSE
			 | NOT of formula
			 | ANDALSO of formula * formula
			 | ORELSE of formula * formula
			 | IMPLY of formula * formula
			 | LESS of expr * expr

let rec expr2num e =
  match e with
  | NUM n -> n
  | PLUS (n1, n2) -> (expr2num n1) + (expr2num n2)
  | MINUS (n1, n2) -> (expr2num n1) - (expr2num n2)

let rec eval f =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT p -> not (eval p)
  | ANDALSO (p, q) -> (eval p) && (eval q)
  | ORELSE (p, q) -> (eval p) || (eval q)
  | IMPLY (p, q) -> (not (eval p) || (eval q))
  | LESS (e1, e2) -> (expr2num e1) < (expr2num e2)
