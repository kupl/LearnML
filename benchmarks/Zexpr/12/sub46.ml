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

    type environment = 	(id * int) list
    type value = int
    
    let emptyEnv = []
    
    let eval (env, e) = 		
    let rec evall (env,e) =
	let rec max nowbig lst = 
		match lst with
		|[] -> nowbig
		|hd::tl -> if (evall (env,hd))>nowbig then (max (evall (env,hd)) tl) else (max nowbig tl)
	in
	let rec check (s,env) = 
		match (s,env) with
		|(_,[]) -> raise (Error "ERROR") 
		|(s,(hd::tl)) -> match (s,hd) with
				 |(s,(a,b)) -> if (s=a) then b
				 	       else check(s,tl)
	in
	match e with
	|NUM a -> a
	|PLUS (e1,e2) -> (evall (env,e1)) + (evall (env,e2))
	|MINUS (e1,e2) -> (evall (env,e1)) - (evall (env,e2))
	|MULT (e1,e2) -> (evall (env,e1)) * (evall (env,e2))
	|DIVIDE (e1,e2) -> (evall (env,e1)) / (evall (env,e2))
	|MAX ([]) -> 0
	|MAX (hd::tl) -> (max (evall (env,hd)) tl)
    	|VAR s -> check(s,env)
	|LET (s,e1,e2) -> (evall (((s,(evall (env,e1)))::env), e2))
in 
let result = evall(env,e)
in
print_int result; result
	
end

    
