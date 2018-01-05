(* 6 eval: formula -> bool *)
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

let rec evalexpr e = match e with
NUM i -> i
| PLUS (e0, e1) -> (evalexpr e0) + (evalexpr e1)
| MINUS (e0, e1) -> (evalexpr e0) - (evalexpr e1)

let rec eval formula = match formula with
TRUE -> true
| FALSE -> false
| NOT f -> not (eval f)
| ANDALSO (f0, f1) -> (eval f0) && (eval f1)
| ORELSE (f0, f1) -> (eval f0) || (eval f1)
| IMPLY (f0, f1) -> not (eval f0) || (eval f1)
| LESS (e0, e1) -> if (evalexpr e0) < (evalexpr e1) then true else false
