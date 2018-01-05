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
	let rec solve expr = 
		match expr with
		| NUM a -> a
		| PLUS (a, b) -> (solve a) + (solve b)
		| MINUS (a, b) -> (solve a) - (solve b) in
	match formula with
	| TRUE -> true
	| FALSE -> false
	| NOT a -> if (eval a) = true then false else true
	| ANDALSO (a, b) -> if (eval a) = true && (eval b) = true then true else false
	| ORELSE (a, b) -> if (eval a) = false && (eval b) = false then false else true
	| IMPLY (a, b) -> if (eval a) = false then true else if (eval b) = true then true else false
	| LESS (a, b) -> if (solve a) < (solve b) then true else false 
