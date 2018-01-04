type formula = TRUE | FALSE | NOT of formula | ANDALSO of formula * formula | ORELSE of formula * formula | IMPLY of formula * formula | LESS of expr * expr
and expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr

let rec  eval fml =
	match fml with
	|TRUE -> true
	|FALSE -> false
	|NOT (fml') -> not (eval fml')
	|ANDALSO (fml1, fml2) -> (eval fml1) && (eval fml2)  
	|ORELSE (fml1, fml2) -> (eval fml1) || (eval fml1)
	|IMPLY (fml1, fml2) -> not(eval fml1) || (eval fml2) 
	|LESS (expr1, expr2) -> 
		let rec tr01 (expr1') =
			match expr1' with
			| NUM (i) ->i 
		 	| MINUS (ex1, ex2) -> tr01(ex1) - tr01(ex2)
			| PLUS (ex1', ex2') -> tr01(ex1') + tr01(ex2')
		in
		let tr02 (expr00, expr01) =
			if tr01(expr00) < tr01(expr01) then true
			else false
		in
	tr02(expr1, expr2)
	 


