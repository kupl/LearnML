exception Error of string
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

let rec eval fo = 
	let rec eval1 ex = 
		match ex with
		NUM(k) -> k
		| PLUS(e1, e2) -> eval1 e1 + eval1 e2
		| MINUS(e1, e2) -> eval1 e1 - eval1 e2
	in
	match fo with 
	TRUE -> true
	| FALSE -> false
	| NOT(f) -> not (eval f) 
	| ANDALSO(f1, f2) -> eval f1 && eval f2
	| ORELSE(f1, f2) -> eval f1 || eval f2
	| IMPLY(f1, f2) -> if eval f1 then eval f2 else true 
	| LESS(e1, e2) -> if eval1 e1 < eval1 e2 then true else false  
