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

let rec func (arg: expr) = 
	match (arg) with 
	| NUM(arg') -> arg'
	| PLUS(left, right) -> (match (left, right) with 
											| (NUM(left'), NUM(right')) -> left' + right'
											| (NUM(left'), _) -> left' + func(right) 
											| (_, NUM(right')) -> func(left) + right'
											| (_, _) -> func(left) + func(right))
	| MINUS(left, right) -> (match (left, right) with 
											 | (NUM(left'), NUM(right')) -> left' - right'
											 | (NUM(left'), _) -> left' - func(right)
											 | (_, NUM(right')) -> func(left) - right'
											 | (_, _) -> func(left) + func(right))

let rec eval (arg: formula) = 
	match (arg) with
	| TRUE -> TRUE
	| FALSE -> FALSE 
  | NOT(arg') -> (match (arg') with 
	 					| (TRUE) -> FALSE
						| (FALSE) -> TRUE
						| (_) -> eval(NOT(eval(arg'))))
	| ANDALSO(f1, f2) -> (match (f1, f2) with  
									   |(TRUE, TRUE) -> TRUE
								 	   |(TRUE, FALSE) -> FALSE
									   |(FALSE, TRUE) -> FALSE
									   |(FALSE, FALSE) -> FALSE
										 |(_, _) -> eval(ANDALSO(eval(f1), eval(f2))))										 
	| ORELSE(f1, f2) -> (match (f1, f2) with 
									  |(FALSE, FALSE) -> FALSE
									  |(TRUE, TRUE) -> TRUE
									  |(TRUE, FALSE) -> TRUE
									  |(FALSE, TRUE) -> TRUE
										|(_, _) -> eval(ORELSE(eval(f1), eval(f2))))
	| IMPLY(f1, f2) -> (match (f1, f2) with 
								   |(TRUE, FALSE) -> FALSE 
								   |(TRUE, TRUE) -> TRUE		 	
								   |(FALSE, TRUE) -> TRUE
	 						     |(FALSE, FALSE) -> TRUE
									 |(_, _) -> eval(IMPLY(eval(f1), eval(f2))))
	| LESS(e1, e2) -> (match (e1, e2) with 
								  |(_, _) -> if func(e1) < func(e2) then TRUE else FALSE)

						 

