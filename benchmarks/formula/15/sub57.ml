type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr;;

let rec eval fin = 
	match fin with
	| TRUE -> true
	| FALSE -> false
	| NOT f1 -> not (eval f1)
	| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	| ORELSE (f1, f2) -> (eval f1) || (eval f2)
	| IMPLY (f1, f2) -> if (eval f1) = false then true
						else if (eval f1) = (eval f2) then true
						else false
	| LESS (e1, e2) ->
		let rec calc exp =
			match exp with
			| NUM x -> x
			| PLUS (e1, e2) -> (calc e1) + (calc e2)
			| MINUS (e1, e2) -> (calc e1) - (calc e2) in
		if (calc e1) < (calc e2) then true
		else false;;
