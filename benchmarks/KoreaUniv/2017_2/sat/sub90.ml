(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let addVar : string -> string list -> string list 
= fun var var_list -> var_list@[var]

let rec getVarNum : string -> string list -> int
= fun var var_list ->
	match var_list with
		| hd::tail -> if (compare hd var) <> 0 then (1 + ( getVarNum var tail ))
			      else 1
		| [] -> 0

let rec getVal n val_list = 
	if n = 1 then match val_list with hd::tail -> hd | [] -> 0
	else match val_list with
		| hd::tail -> getVal (n-1) tail
		| [] -> 0

let cal_AND p q = 
	if (p = 1) && (q = 1) then 1
	else 0

let cal_OR p q = 
	if (p = 1) || (q = 1) then 1
	else 0

let cal_NEG p = 
	if p = 1 then 0
	else 1

let cal_Imply p q = if (p = 1) && (q = 1) then 1
		    else if (p = 1) && (q = 0) then 0
		    else if (p = 0) && (q = 1) then 1
		    else 1

let cal_Iff : int -> int -> int
= fun p q -> if p = q then 1
		  else 0

let rec makeVarList form var_list =
	match form with
		| Var var -> if (getVarNum var var_list) = 0 then [var]
			     else []
		| And (f1, f2) -> (makeVarList f1 var_list)@(makeVarList f2 var_list)
		| Or (f1, f2) -> (makeVarList f1 var_list)@(makeVarList f2 var_list)
		| Iff (f1, f2) -> (makeVarList f1 var_list)@(makeVarList f2 var_list)
		| Imply (f1, f2) -> (makeVarList f1 var_list)@(makeVarList f2 var_list)
		| Neg (f) -> makeVarList f var_list
		| _ -> []

let rec calculate target var_list val_list = 
	match target with
		| Var var -> getVal (getVarNum var var_list) val_list
		| And (f1, f2) -> cal_AND (calculate f1 var_list val_list) (calculate f2 var_list val_list)
		| Or (f1, f2) -> cal_OR (calculate f1 var_list val_list) (calculate f2 var_list val_list)
		| Iff (f1, f2) -> cal_Iff (calculate f1 var_list val_list) (calculate f2 var_list val_list)
		| Imply (f1, f2) -> cal_Imply (calculate f1 var_list val_list) (calculate f2 var_list val_list)
		| Neg (f) -> cal_NEG (calculate f var_list val_list)
		| True -> 1
		| False -> 0

let rec calAllCases formul var_list val_list var_left = 
	match var_left with
		| [] -> calculate formul var_list val_list
		| hd::tail -> (calAllCases formul var_list (val_list@[1]) tail) + (calAllCases formul var_list (val_list@[0]) tail)

let sat : formula -> bool
= fun f -> 
	let var_list = makeVarList f []
	in if (calAllCases f var_list [] var_list) = 0 then false
	   else true