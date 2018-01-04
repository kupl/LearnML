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


let rec eval formula =
	let rec calc_expr expr = 
		match expr with
		| NUM x -> x
		| PLUS (x, y) -> (calc_expr x) + (calc_expr y)
		| MINUS (x, y) -> (calc_expr x) - (calc_expr y) in
	
	match formula with
	| TRUE -> true
	| FALSE -> false
	| NOT x -> if (eval x)=true then false
				else true
	| ANDALSO (x, y) -> (eval x) && (eval y)
	| ORELSE (x, y) -> (eval x) || (eval y)
	| IMPLY (x, y) -> if (eval x)=false then true
					else if (eval y)=true then true
					else false
	| LESS (x, y) -> if (calc_expr x)<(calc_expr y) then true
					else false

