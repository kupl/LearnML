(* Ex 4. True or False *)
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

let rec calc_expr e =
	match e with
	| NUM e -> e
	| PLUS (e1, e2) -> (calc_expr e1) + (calc_expr e2)
	| MINUS (e1, e2) -> (calc_expr e1) - (calc_expr e2)

let rec eval f =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT sf -> not (eval sf)
	| ANDALSO (sf1, sf2) -> (eval sf1) && (eval sf2)
	| ORELSE (sf1, sf2) -> (eval sf1) || (eval sf2)
	| IMPLY (sf1, sf2) ->
		(match (eval sf1) with
		| true -> eval sf2
		| false -> true)
	| LESS (e1, e2) ->
		(if (calc_expr e1) < (calc_expr e2) then true
		else false)
(*
let _ =
	let msg = string_of_bool (eval TRUE) in
	print_endline msg

let _ =
	let msg = string_of_bool (eval (ANDALSO(TRUE, FALSE))) in
	print_endline msg

let _ =
	let msg = string_of_bool (eval (LESS(NUM 3, PLUS(NUM 2, NUM 4)))) in
	print_endline msg
*)