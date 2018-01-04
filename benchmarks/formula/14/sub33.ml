exception TODO

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

let rec value (e: expr) : int =
	match e with
	| NUM t -> t
	| PLUS (t,z) -> (value t) + (value z)
	| MINUS (t,z) -> (value t) - (value z)

let rec eval (f: formula) : bool =
	match f with
	| TRUE -> true 
	| FALSE -> false
	| ANDALSO (x,y) -> (eval x) && (eval y)
	| ORELSE (x,y) -> (eval x) || (eval y)
	| IMPLY (x,y) -> not (eval x) || (eval y)
	| LESS (x,y) -> (value x) < (value y)
	| NOT f -> not (eval f) 
