open Lang
open Util

(* If the given program has infinite loop, return true *)

module Static = struct
	
	let name_of_func : let_bind -> id
	= fun f ->
		match f with
		| BindOne f -> f
		| _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")

	let rec is_arg : id -> arg -> bool
	= fun f arg ->
		match arg with
  	| ArgOne (x, _) when x = f -> true
 		| ArgTuple xs -> List.exists (is_arg f) xs
 		| _ -> false

	let rec is_bind : id -> let_bind -> bool
	= fun f x ->
		match x with
  	| BindOne x when x = f -> true
 		| BindTuple xs -> List.exists (is_bind f) xs
 		| _ -> false

	let rec check_exp : id -> exp -> bool
	= fun f exp ->
		match exp with
		| EList es | ETuple es | ECtor (_, es) -> List.exists (check_exp f) es
    | MINUS e | NOT e | Raise e -> check_exp f e
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) | OR (e1, e2) | AND (e1, e2) 
    | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2) 
    | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) -> (check_exp f e1) || (check_exp f e2)
    | EApp (e1, e2) -> 
    	begin match e1 with
    	| EVar x when x = f -> true
    	| _ -> check_exp f e2
    	end
    | EFun (arg, e) -> if is_arg f arg then false else check_exp f e
    | ELet (f', is_rec, args, typ , e1, e2) -> 
    	if is_bind f f' then
    		check_exp (name_of_func f') e1
    	else
    		check_exp (name_of_func f') e1 || check_exp f e2
    | EBlock (is_rec, bindings, e2) ->
    	if (List.exists (fun (f', is_rec, args, typ, e) -> is_bind f f') bindings) then
    		(List.exists (fun (f', is_rec, args, typ, e) -> if is_rec then check_exp (name_of_func f') e else false) bindings)
    	else
    		(List.exists (fun (f', is_rec, args, typ, e) -> if is_rec then check_exp (name_of_func f') e else false) bindings) || check_exp f e2
    | EMatch (e, bs) ->
    	let (_, es) = List.split bs in
    	if (check_exp f e) then true else List.for_all (check_exp f) es
    | IF (e1, e2, e3) -> if (check_exp f e1) then true else (check_exp f e2) && (check_exp f e3)
    | _ -> false

	let rec check_decl : decl -> bool
	= fun decl ->
		match decl with
    | DLet (f, is_rec, args, typ, e) -> if is_rec then check_exp (name_of_func f) e else false 
    | DBlock (is_rec, bindings) -> List.exists (fun binding -> check_decl (DLet binding)) bindings
    | _ -> false

	let run : prog -> bool
	= fun pgm -> List.exists (check_decl) pgm

end