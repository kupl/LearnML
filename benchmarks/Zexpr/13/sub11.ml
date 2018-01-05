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
    
    val emptyEnv : environment
    val eval : environment * expr -> value

    val int_of_value : value -> int
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
    type value = VAL of int
    type environment = EMPTY_ENV | ENV of id * value * environment
    
    let emptyEnv = EMPTY_ENV
    let int_of_value = function(VAL x) -> x
	
    let rec eval (env, e) = match e with
		NUM x -> VAL x
		| PLUS (a,b) -> VAL (int_of_value(eval(env,a)) + int_of_value(eval(env,b)))
		| MINUS (a,b) -> VAL (int_of_value(eval(env,a)) - int_of_value(eval(env,b)))
		| MULT (a,b) -> VAL (int_of_value(eval(env,a)) * int_of_value(eval(env,b)))
		| DIVIDE (a,b) -> VAL (int_of_value(eval(env,a)) / int_of_value(eval(env,b)))
		| MAX  lst -> (
			match lst with
			[] -> VAL 0
			| _ -> VAL (List.hd (List.sort (fun a b -> if(a=b) then 0 else if(a<b) then 1 else -1) (List.map (fun x -> int_of_value(eval(env,x))) lst)))
		)
		| VAR x -> (
			match env with
			EMPTY_ENV -> raise (Error "FreeVariable")
			| ENV (id, value, env_parent) -> if(id=x) then value else eval(env_parent, e)
		)
		| LET (x,x_exp,exp) -> eval(ENV (x, eval(env,x_exp), env), exp)
	
end
