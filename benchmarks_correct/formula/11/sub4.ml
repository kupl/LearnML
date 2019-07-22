(*2006-11681 °­Çö¼®*)
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
	
	let rec calc e =
		match e with
		NUM n -> n
		| PLUS(n0,n1) -> (calc n0) + (calc n1)
		| MINUS(n0,n1) -> (calc n0) - (calc n1)
	in

	match f with
	TRUE -> true 
	| FALSE -> false
	| NOT f0 -> not (eval f0)
	| ANDALSO(f0,f1) -> (eval f0) && (eval f1)
	| ORELSE(f0,f1) -> (eval f0) || (eval f1)
	| IMPLY(f0,f1) -> (match (eval f0, eval f1) with
						(_,true) -> true 
						| (true, false) -> false
						| (false, false) -> true)
	| LESS(e0,e1) -> (if (calc e0)<(calc e1) then true
					   else false)
