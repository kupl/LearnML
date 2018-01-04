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

let rec eval fm =

	let rec exprcalc e =
		match e with 
		| NUM n -> n
		| PLUS (f1, f2) -> (exprcalc f1) + (exprcalc f2)
		| MINUS (f1, f2) -> (exprcalc f1) + (exprcalc f2)
	in

	match fm with
	| TRUE -> true
	| FALSE -> false
	| NOT f -> not (eval f)
	| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	| ORELSE (f1, f2) -> (eval f1) || (eval f2)
	| IMPLY (f1, f2) -> if (eval f1)==true && (eval f2)==false then false else true
	| LESS (f1, f2) -> if(f1<f2) then true else false

;;