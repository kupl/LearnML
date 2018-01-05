(* ex5 *)
module type ZEXPR = 
sig
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

module Zexpr : ZEXPR = 
	struct
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
		type value = int
		type environment = (id * value) list
		let emptyEnv = []
		
		let eval (env, e) = 
			let rec ev (env, ex) = 
				
				let rec mapEval (envr, lst) = 
					match lst with
					[] -> []
					| hd::tl -> (ev (envr, hd))::(mapEval (envr, tl)) in
					
				let pickVar (var, valu) = var in
				let pickVal (var, valu) = valu in
				
				let rec searchVarVal (envr, v)  = 
					match envr with
					[] -> raise (Error "no such variable in this scope")
					| hd::tl -> if (pickVar hd) = v then pickVal hd
								else searchVarVal (tl, v) in
				
				match ex with
				NUM i -> i
				| PLUS (e1, e2) -> (ev (env, e1)) + (ev (env, e2))
				| MINUS (e1, e2) -> (ev (env, e1)) - (ev (env, e2))
				| MULT (e1, e2) -> (ev (env, e1)) * (ev (env, e2))
				| DIVIDE (e1, e2) -> if (ev (env, e2)) = 0 then raise (Error "divide by zero")
									else (ev (env, e1)) / (ev (env, e2))
				| MAX lst -> if lst = [] then 0
							else List.nth (List.sort compare (mapEval (env, lst))) ((List.length lst) - 1)
				| VAR v -> searchVarVal (env, v)
				| LET (v, e1, e2) -> ev ((v, ev (env, e1))::env, e2)
			in
			
			let i = ev (env, e) in
			print_int i;
			i
	end
