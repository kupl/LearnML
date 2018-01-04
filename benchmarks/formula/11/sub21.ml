(* 컴퓨터공학부/2009-11679/김정명/5 *)

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

let rec eval form =
	let rec calc e = 
		match e with
		  NUM n -> n
		| PLUS (e1, e2) -> calc e1 + calc e2
		| MINUS (e1, e2) -> calc e1 - calc e2
	in

	match form with 
	  TRUE -> true
	| FALSE -> false
	| NOT f -> not (eval f)
	| ANDALSO (f1, f2) -> 
		if eval f1 && eval f2 then true
		else false
	| ORELSE (f1, f2) ->
		if eval f1 || eval f2 then true
		else false
	| IMPLY (f1, f2) ->
		if eval f1 && not (eval f2) then false
		else true
	| LESS (e1, e2) ->
		if calc e1 < calc e2 then true
		else false
