type formula = True
	    | False
	    | Not of formula
	    | AndAlso of formula * formula
	    | OrElse of formula * formula
	    | Imply of formula * formula
	    | Equal of exp * exp


and exp = Num of int
	 | Plus of exp * exp
	 | Minus of exp * exp;;

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
	if (e1 = e2) then true
	else false ;;

let rec val_exp e =
	match e with
	Num k -> k
	| Plus (e1, e2) -> (val_exp e1) + (val_exp e2)
	| Minus (e1, e2) -> (val_exp e1) - (val_exp e2);;



let rec eval form =
	match form with
	True -> true
	| False -> false
	| Not f -> val_not (eval f) 
	| AndAlso (f1,f2) -> val_and (eval f1) (eval f2)
	| OrElse (f1, f2) -> val_or (eval f1) (eval f2)
	| Imply (f1, f2) -> val_imply (eval f1) (eval f2)
	| Equal (e1, e2) -> val_less (val_exp e1) (val_exp e2);;

