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

exception Error of string

let rec val_and f1 f2 =
	match (f1,f2) with
	(true,true) -> true
 	| _ -> false ;;

let rec val_or f1 f2 =
	match (f1, f2) with
	(false, false) -> false
	| _ -> true ;;

let rec val_imply f1 f2 =
	match (f1, f2) with
	(true, false) -> false
	| _ -> true ;;

let rec val_not f =
	match f with
	true -> false
	| false -> true


let rec val_less e1 e2 =
	if (e1 < e2) then true
	else false ;;

let rec val_expr e =
	match e with
	NUM k -> k
	| PLUS (e1, e2) -> (val_expr e1) + (val_expr e2)
	| MINUS (e1, e2) -> (val_expr e1) - (val_expr e2);;



let rec eval form =
	match form with
	TRUE -> true
	| FALSE -> false
	| NOT f -> val_not (eval f) 
	| ANDALSO (f1,f2) -> val_and (eval f1) (eval f2)
	| ORELSE (f1, f2) -> val_or (eval f1) (eval f2)
	| IMPLY (f1, f2) -> val_imply (eval f1) (eval f2)
	| LESS (e1, e2) -> val_less (val_expr e1) (val_expr e2);;

