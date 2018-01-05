module type ZEXPR = sig 
	exception Error of string 
	type id = string 
	type expr = NUM of int 
			| PLUS of expr * expr 
			| MINUS of expr * expr 
			| MULT of expr * expr 
			| DIVIDE of expr * expr 
			| MAX of expr list 
			| VAR of id 
			| LET of id * expr * expr 
	type environment 
	type value 
	val emptyEnv: environment 
	val eval: environment * expr -> value 
end

module Zexpr : ZEXPR = struct
	exception Error of string 
	type id = string 
	type expr = NUM of int 
			| PLUS of expr * expr 
			| MINUS of expr * expr 
			| MULT of expr * expr 
			| DIVIDE of expr * expr 
			| MAX of expr list 
			| VAR of id 
			| LET of id * expr * expr 
	type environment = (id * int) list
	type value = int
	let emptyEnv = []
	let eval (env, expr) = 
		let rec do_eval (env, expr) =
			let rec do_eval_list env l =
				match l with
				[] -> [0]
				| h::t -> List.append [do_eval (env, h)] (do_eval_list env t)
			in
			
			match expr with
			NUM e -> e;
			| PLUS (e1, e2) -> do_eval(env, e1) + do_eval(env, e2)
			| MINUS (e1, e2) -> do_eval(env, e1) - do_eval(env, e2)
			| MULT (e1, e2) -> do_eval(env, e1) * do_eval(env, e2)
			| DIVIDE (e1, e2) -> if (0 == do_eval(env, e2)) then raise (Error "Divide 0")
									else do_eval(env, e1) / do_eval(env, e2)
			| MAX l -> List.hd( List.rev (List.sort compare (do_eval_list env l)))
			| VAR id -> if (List.mem_assoc id env) then List.assoc id env
						else raise (Error "Value Not Found")
			| LET (id, e1, e2) -> if (List.mem_assoc id env) then do_eval((List.append (List.remove_assoc id env) [(id, do_eval(env, e1))]) , e2)
									else do_eval((List.append env [(id, do_eval(env, e1))]) , e2)
		in
	
		let result = (do_eval (env, expr))
		in
		
		print_int result;
		result
end