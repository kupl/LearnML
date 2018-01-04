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


let rec calc exp =
	match exp with
	| NUM exp -> exp
	| PLUS(expa, expb) -> (calc expa) + (calc expb)
	| MINUS(expa, expb) -> (calc expa) - (calc expb)

let rec eval exp = 
	match exp with 
	| TRUE -> true
	| FALSE -> false
	| NOT expa -> not (eval expa)
	| ANDALSO(expa, expb) -> ( (eval expa) && (eval expb) )
	| ORELSE(expa, expb) -> ( (eval expa) || (eval expb) )
	| IMPLY(expa, expb) -> ( (not (eval expa) ) || (eval expb) )
	| LESS(expa, expb) -> 
		if (calc expa) < (calc expb) then true
		else false
