type formula = 
TRUE 
| FALSE 
| NOT of formula 
| ANDALSO of formula * formula 
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec num exp = 
	match exp with
	| NUM x -> x
	| PLUS (x, y) -> (num x) + (num y)
	| MINUS (x, y) -> (num x) - (num y)

let rec eval form =
	match form with 
	| TRUE -> true
	| FALSE -> false
	| NOT x -> not (eval x)
	| ANDALSO(x, y) -> if( (eval x) = true && (eval y) = true) then true else false
	| ORELSE(x, y) -> if( (eval x) = false && (eval y) = false) then false else true
	| IMPLY(x, y) -> if( (eval x) = true && (eval y) = false ) then false else true
	| LESS(x, y) -> if( (num x) < ( num y)) then true else false
	
