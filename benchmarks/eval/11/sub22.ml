
(* ex 3 *)

type expr = NUM of int	
		  | PLUS of expr * expr
		  | MINUS of expr * expr
		  | MULT of expr * expr
		  | DIVIDE of expr * expr
		  | MAX of expr list


let rec eval exp =
	let rec find_max(m,l) =
		match l with
		| [] -> m 
		| h::t -> if ((eval h)>m) then find_max((eval h),t) else find_max(m,t)
	in

	let find_max_init l= 
		match l with
		| [] -> 0 
		| h::t -> find_max((eval h),t)		
	in
 
	match exp with
	| NUM n -> n
	| PLUS(e1,e2) -> (eval e1) + (eval e2)
	| MINUS(e1,e2) -> (eval e1) - (eval e2)
	| MULT(e1,e2) -> (eval e1) * (eval e2)
 	| DIVIDE(e1,e2) -> (eval e1) / (eval e2)
	| MAX l -> (find_max_init l)
(*	| _ -> raise (Invalid_argument "eval") *)
 
