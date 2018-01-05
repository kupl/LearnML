module type ZEXPR = sig
    
    exception Error of string 
    type id = string
    type expr = 
        | NUM of int
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
    type expr = 
        | NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr
        | MULT of expr * expr
        | DIVIDE of expr * expr
        | MAX of expr list
        | VAR of id
        | LET of id * expr * expr

    type environment = id -> int 
    type value = unit
    
    let emptyEnv = fun x -> raise (Error "The variable is not found in the environment\n")
 
		let eval (env, expr) = 
			let rec eval_sub (env, expr) = match (env, expr) with
				| (_, NUM i) -> i
				| (env, PLUS (e1, e2)) -> eval_sub(env, e1) + eval_sub(env, e2)
				| (env, MINUS (e1, e2)) -> eval_sub(env, e1) - eval_sub(env, e2)
				| (env, MULT (e1, e2)) -> eval_sub(env, e1) * eval_sub(env, e2)
				| (env, DIVIDE (e1, e2)) -> let div = eval_sub(env, e2)
																		in 
																			if div = 0 then raise (Error "Divided by 0\n")
																			else eval_sub(env, e1) / div
				| (env, MAX elist ) ->
					let rec findmax(env, elist) = match elist with
						[] -> 0
						| h::[] -> eval_sub (env, h)
						| h::t -> if eval_sub(env, h) > findmax(env, t) then eval_sub(env, h)
											else findmax(env, t)
					in
						findmax(env, elist) 
			 	| (env, VAR id) -> env id
				| (env, LET (id, e1, e2)) -> 
					let addvar env id value =
						fun x -> if x = id then value
										 else env x
					in
						eval_sub ((addvar env id (eval_sub (env, e1))), e2)	
			in print_int (eval_sub(env, expr));print_newline()
	    
end

    
