(* define the type *)

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

	
			
(* define the operator *)

let rec expr_to_int e =
	match e with
		  NUM i -> i
		| PLUS (e1, e2) -> (expr_to_int e1) + (expr_to_int e2)
		| MINUS (e1, e2) -> (expr_to_int e1) - (expr_to_int e2)

let less_cmd (e1, e2) =
	if (expr_to_int e1) < (expr_to_int e2) then TRUE
	else FALSE
		
let and_also_cmd (f1, f2) = 
	(* If both expressions evaluate to True, result is True. *)
	match (f1, f2) with
		  (TRUE, TRUE) -> TRUE
		| _ -> FALSE

let or_else_cmd (f1, f2) =
	(* If either or both expressions evaluate to True, result is True.*)
	match (f1, f2) with
		  (FALSE, FALSE) -> FALSE
		| _ -> TRUE
		
let imply_cmd (f1, f2) = 
	match (f1, f2) with
		  (TRUE, FALSE) -> FALSE
		| _ -> TRUE

let rec not_cmd f =
	match f with
		  TRUE -> FALSE
		| FALSE -> TRUE
		| NOT innerF -> innerF
		| ANDALSO (f1, f2) -> not_cmd (and_also_cmd (f1, f2)) 
		| ORELSE (f1, f2) -> not_cmd (or_else_cmd (f1, f2))
		| IMPLY (f1, f2) -> not_cmd (imply_cmd (f1, f2))
		| LESS (e1, e2) -> not_cmd (less_cmd (e1, e2))


let rec formal_to_bool f =
	match f with
		  TRUE -> true
		| FALSE -> false
		| NOT innerF -> formal_to_bool (not_cmd f)
		| ANDALSO (f1, f2) -> formal_to_bool (and_also_cmd (f1, f2))
		| ORELSE (f1, f2) -> formal_to_bool (or_else_cmd (f1, f2))
		| IMPLY (f1, f2) -> formal_to_bool (imply_cmd (f1, f2))
		| LESS (e1, e2) -> formal_to_bool (less_cmd (e1, e2))



(* Define the function eval *)

let eval f =
	match f with
		  TRUE -> true
		| FALSE -> false
		| NOT innerF -> formal_to_bool (not_cmd innerF)
		| ANDALSO (f1, f2) -> formal_to_bool (and_also_cmd (f1, f2))
		| ORELSE (f1, f2) -> formal_to_bool (or_else_cmd (f1, f2))
		| IMPLY (f1, f2) -> formal_to_bool (imply_cmd (f1, f2))
		| LESS (e1, e2) -> formal_to_bool (less_cmd (e1, e2))


	
