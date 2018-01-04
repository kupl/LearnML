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

let rec eval t =
	let rec inteval x =
		match x with
		PLUS(x1, x2) -> (inteval x1) + (inteval x2)
		|MINUS(x1, x2) -> (inteval x1) - (inteval x2)
		|NUM x -> x
		in
	
	match t with
	NOT t -> not (eval t)
	|ANDALSO (t1, t2) -> (eval t1) && (eval t2)
	|ORELSE (t1, t2) -> (eval t1) || (eval t2)
	|IMPLY (t1, t2) -> (not (eval t1)) || (eval t2)
	|LESS (t1, t2) -> (inteval t1) < (inteval t2)
	|TRUE -> true
	|FALSE -> false
