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

let rec eval f =
	let rec eval2 e =
		match e with (NUM n) -> n
			| (PLUS (n1, n2)) -> (eval2 n1) + (eval2 n2)
			| (MINUS (n1, n2)) -> (eval2 n1) - (eval2 n2) in
	match f with TRUE -> true
			| FALSE -> false
			| (NOT f2) -> (eval f2)
			| (ANDALSO (f1, f2)) -> (eval f1) & (eval f2)
			| (ORELSE (f1, f2)) -> (eval f1) || (eval f2)
			| (IMPLY (f1, f2)) -> (if (eval f1) then (eval f2) 
						else true)
			| (LESS (e1, e2)) -> (eval2 e1) < (eval2 e2)