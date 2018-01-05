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

    type environment = (id * value) list
			
	
    type value = int
    
    let emptyEnv = []
    let rec eval (env, e) = 
			match e with
				| NUM i -> i
				| VAR v -> (match env with
					| hd::tl -> let (id,value) = hd in
					if v = id then value
					else eval (tl, e)
					| [] -> raise (Error "Undefinded id") )
				| PLUS (e1,e2) -> (eval (env,e1)) + (eval (env,e2))
				| MINUS (e1,e2) -> (eval (env,e1)) - (eval (env,e2))
				| MULT (e1,e2) -> (eval (env,e1)) * (eval (env,e2))
				| DIVIDE (e1,e2) -> 
					let divider = eval (env,e2) in
					if divider = 0 then raise (Error "divide with zero")
					else (eval (env,e1)) / divider
				| MAX expl -> (match expl with
					| hd::[] -> eval (env,hd)
					| hd::tl -> 
						let temp = eval (env,hd) in
						let tempmax = eval (env,MAX tl) in
						if temp > tempmax then temp else tempmax 
					| [] -> 0 )
				| LET (id,e1,e2) -> 
					let value = eval (env, e1) in
					eval ((id,value)::env, e2)
					
end

    