(* Ex 5 *)
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

    type environment = (id * expr) list
    type value = int

    let emptyEnv = []


    let rec env_traverse (env, x)	= match env with
					| [] -> raise (Error "No such definition exists")
					| (k, e)::tl -> if k = x
							then e
							else env_traverse (tl, x)

    let rec aux_eval (env, e) = 
        let rec max_eval (env, lst, t)	= match lst with
					| [] -> t
					| hd::tl ->	if t > aux_eval (env, hd)
							then max_eval (env, tl, t)
							else max_eval (env, tl, aux_eval (env, hd))
	in
				match e with
			      | NUM(i) -> i
			      | PLUS(e1, e2) -> aux_eval(env, e1) + aux_eval(env, e2)
			      | MINUS(e1, e2) -> aux_eval(env, e1) - aux_eval(env, e2)
			      | MULT(e1, e2) -> aux_eval(env, e1) * aux_eval(env, e2)
			      | DIVIDE(e1, e2) -> aux_eval(env, e1) / aux_eval(env, e2)
			      | MAX(lst) -> max_eval (env, lst, 0)
			      | VAR(x) -> aux_eval (env, env_traverse (env, x))
			      | LET(x, e1, e2) -> aux_eval ((x, e1)::env, e2)

    let eval (env, e) = print_int (aux_eval (env, e)) ; aux_eval (env, e)

end