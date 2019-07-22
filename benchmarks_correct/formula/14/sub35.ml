(* not tested *)

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

let rec exp_to_num e = 
	match e with
	| NUM i -> i
	| PLUS (e1, e2) -> (exp_to_num e1) + (exp_to_num e2)
	| MINUS (e1, e2) -> (exp_to_num e1) - (exp_to_num e2)

let rec eval (v: formula) =
	match v with
	| TRUE -> true
	| FALSE -> false
	| NOT f -> not (eval f)
	| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	| ORELSE (f1, f2) -> (eval f1) || (eval f2)
	| IMPLY (f1, f2) -> if ((eval f1) = true) && ((eval f2) = false) then false else true
	| LESS (e1, e2) -> if (exp_to_num e1) < (exp_to_num e2) then true else false
