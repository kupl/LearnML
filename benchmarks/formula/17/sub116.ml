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

let rec exprval (expr1:expr) : int =
							   match expr1 with
											   NUM (n) -> n
											  |PLUS(expr1,expr2) -> exprval expr1 + exprval expr2
											  |MINUS(expr1,expr2) -> exprval expr1 - exprval expr2
let rec eval (f:formula) : bool =
					   match f with
					   TRUE -> true
					  |FALSE -> false
					  |NOT (formula1) -> if(eval formula1 == true) then false else true
					  |ANDALSO (formula1,formula2) -> if(eval formula1 == true && eval formula2 == true) then true else false
					  |ORELSE (formula1,formula2) -> if(eval formula1 == true || eval formula2 == true) then true else false
					  |IMPLY (formula1,formula2) -> if(eval formula1 == true && eval formula2 == false) then false else true
					  |LESS (expr1,expr2) -> if(exprval expr1 < exprval expr2) then true else false

