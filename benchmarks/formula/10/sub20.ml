
(*Ex5*)
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
let rec eval : formula -> bool =
	let rec evale : expr -> int =
		fun ex -> match ex with NUM a -> a
							|PLUS(a,b) -> evale(a) + evale(b)
							|MINUS(a,b) -> evale(a) - evale(b)
	in
	fun ex -> match ex with TRUE -> true
						|	FALSE -> false
						|	NOT ex -> not(eval(ex))
						|	ANDALSO (a,b) -> eval(a) & eval(b)
						|	ORELSE (a,b) -> eval(a) || eval(b)
						|	IMPLY (a,b) -> if eval(b) then true else
											if eval(a) then false else true
						|	LESS (a,b) -> evale(a) < evale(b)
