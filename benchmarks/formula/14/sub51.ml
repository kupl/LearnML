type formula = 
	TRUE 
	| FALSE 
	| NOT of formula 
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr =
	NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	
let rec calc e =
	match e with
	| NUM n -> n
	| PLUS(e1,e2) -> (calc e1) + (calc e2)
	| MINUS(e1,e2) -> (calc e1) - (calc e2)
	
let rec eval form =
	match form with
	| TRUE -> true
	| FALSE -> false
	| NOT f1 -> if eval f1 = true then false else true
	| ANDALSO(f1,f2) -> if eval f1 = true then eval f2 else false
	| ORELSE(f1,f2) -> if eval f1 = false then eval f2 else true
	| IMPLY(f1,f2) -> if eval f1 = true then eval f2 else true 
	| LESS(e1,e2) -> if calc e1 < calc e2 then true else false