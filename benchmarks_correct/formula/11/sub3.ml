(* 200511843 LEE JONGHO *)

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


let imply (f1, f2) =
	if f1 = true then if f2 = true then true else false
	             else true

let orelse (f1, f2) =
	if f1 = true || f2 = true then true
				  else false

let andalso (f1, f2) =
	if f1 = true && f2 = true then true
				  else false


let less (e1, e2) =
	let rec cal ex =
		match ex with	
		NUM x -> x
		| PLUS (e1, e2) -> (cal e1)+(cal e2)
		| MINUS (e1, e2) -> (cal e1)-(cal e2) in
	if (cal e1) < (cal e2) then true
		               else false

 

let rec eval f =
	match f with
	TRUE -> true
	| FALSE -> false
	| NOT fom -> not (eval fom)
	| IMPLY (f1, f2) -> imply ((eval f1), (eval f2))
	| LESS (e1, e2) -> less (e1, e2)
	| ORELSE (f1, f2) -> orelse ((eval f1), (eval f2))
	| ANDALSO (f1, f2) -> andalso ((eval f1), (eval f2))
