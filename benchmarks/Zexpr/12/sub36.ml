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

    type environment = (id * int) list
    type value = int
    
    let emptyEnv = []
   	
	let eval (env, e) = 
		
		let rec subeval (env, e) =
			
			let rec maxfunc m l env = 
				match l with
				| [] -> m
				| (hd::tl) -> if subeval(env, hd) > m then maxfunc (subeval(env, hd)) tl env
							  else maxfunc m tl env
			in
		
			let rec findme env i =
				match env with
				| [] -> raise (Error "no id in environment")
				| (hd::tl) ->
					(match hd with
					| (a, b) -> if a = i then b else findme tl i
					)
			in

			let rec letfunc i e1 e2 env orihd flag =
				match env with
				| [] -> subeval ([(i, (subeval(env, e1)))], e2)
				| (hd::tl) -> 
					if hd = orihd && flag = 1 then subeval (((i, (subeval (env, e1)))::env), e2)
					else
					(match hd with
					| (a, b) -> if a = i then subeval (((i, (subeval (env, e1)))::tl), e2) else letfunc i e1 e2 (List.append tl [hd]) orihd 1
					)
			in
			
			match e with
			| NUM a -> a
			| PLUS (e1, e2) -> subeval(env, e1) + subeval(env, e2)
			| MINUS (e1, e2) -> subeval(env, e1) - subeval(env, e2)
			| MULT (e1, e2) -> subeval(env, e1) * subeval(env, e2)
			| DIVIDE (e1, e2) -> if subeval(env, e2) = 0 then raise (Error "divided by zero")
								else (subeval(env, e1)) / (subeval(env, e2))
			| MAX [] -> 0
			| MAX (hd::tl) -> maxfunc (subeval (env, hd)) tl env
			| VAR i -> findme env i
			| LET (i, e1, e2) -> if env = [] then subeval ([(i, (subeval(env, e1)))], e2) else letfunc i e1 e2 env (List.hd env) 0
		in

    print_int(subeval(env, e)); subeval(env, e)
end
