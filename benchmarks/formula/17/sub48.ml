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

let rec eval_number f =
	match f with
	| NUM f -> f
	| PLUS(f1, f2) -> (eval_number f1)+(eval_number f2)
	| MINUS(f1, f2) -> (eval_number f1)-(eval_number f2)
	
let rec eval_formula (f:formula) = 
	match f with
	| TRUE -> TRUE
	| FALSE -> FALSE
	| NOT TRUE -> FALSE
	| NOT FALSE -> TRUE
	| NOT (f1) -> eval_formula(NOT (eval_formula f1))
	| ANDALSO(TRUE, TRUE) -> TRUE
	| ANDALSO(FALSE, _) -> FALSE
	| ANDALSO(_, FALSE) -> FALSE
	| ANDALSO((f1), (f2)) -> eval_formula(ANDALSO((eval_formula f1), (eval_formula f2)))
	| ORELSE(TRUE, _) -> TRUE
	| ORELSE(_, TRUE) -> TRUE
	| ORELSE(FALSE, FALSE) -> FALSE
	| ORELSE((f1), (f2)) -> eval_formula(ORELSE((eval_formula f1), (eval_formula f2)))
	| IMPLY(TRUE, TRUE) -> TRUE
	| IMPLY(TRUE, FALSE) -> FALSE
	| IMPLY(FALSE, _) -> TRUE
	| IMPLY((f1), (f2)) -> eval_formula(IMPLY((eval_formula f1), (eval_formula f2)))
    | LESS((f1), (f2)) -> if ((eval_number f1)< (eval_number f2)) then TRUE
    									else FALSE
let eval (f:formula)=
	if((eval_formula f) == TRUE) then true
	else false

