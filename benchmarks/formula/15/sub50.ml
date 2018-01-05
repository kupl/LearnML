type formula 	= TRUE
				| FALSE
				| NOT of formula
				| ANDALSO of formula * formula
				| ORELSE of formula * formula
				| IMPLY of formula * formula
				| LESS of expr * expr
	and expr 	= NUM of int
				| PLUS of expr * expr
				| MINUS of expr * expr

let rec calc : expr -> int = function expr ->
     match expr with
     | NUM i -> i
     | PLUS (exp1, exp2) -> calc exp1 + calc exp2
     | MINUS (exp1, exp2) -> calc exp1 - calc exp2


let rec eval : formula -> bool = function formula ->
	match formula with
	| TRUE -> true
	| FALSE -> false
	| NOT form1 -> not (eval form1)
	| ANDALSO (form1, form2) -> (eval form1) && (eval form2)
	| ORELSE (form1, form2) -> (eval form1) || (eval form2)
	| IMPLY (form1, form2) -> (not (eval form1)) || (eval form2)
	| LESS (exp1, exp2) -> if calc exp1 < calc exp2 then true else false
