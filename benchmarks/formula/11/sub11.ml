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

let rec cal expr = 
	match expr with
	| NUM x -> x
	| PLUS (x, y) -> cal x + cal y
	| MINUS (x, y) -> cal x - cal y

let rec eval formula =
	match formula with
		| TRUE -> true
		| FALSE -> false
		| NOT x -> (match x with
					| TRUE -> false
					| FALSE -> true
					| _ -> eval x
				)
		| ANDALSO (x,y) -> if x!=TRUE || x!=FALSE then eval x
							else if y!=TRUE || y!=FALSE then eval y
							else (match (x,y) with
									| (TRUE, TRUE) -> true
									| (_, _) -> false
								 )
		| ORELSE (x,y) -> if x!=TRUE || x!=FALSE then eval x
							else if y!=TRUE || y!=FALSE then eval y
							else (match (x,y) with
									| (FALSE, FALSE) -> false
									| (_, _) -> true
								 )
		| IMPLY (x,y) -> if x!=TRUE || x!=FALSE then eval x
							else if y!=TRUE || y!=FALSE then eval y
							else if x = FALSE then true
							else if y = TRUE then true
							else false
		| LESS (e1,e2) -> if cal e1 < cal e2 then true
						else false
