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
    
    type value = int
    type environment = (string * int) list
    
    let emptyEnv = []
    let eval (env, e) : value = 
	let rec evalu (env, e) =
	    let rec check (env, v) =
		match env with
		| [] -> false
		| (env1_s, _)::evn2 -> 
		    if (env1_s = v) then true
		    else check (evn2, v)
	    in

	    let rec getvalue (env, v) =
		match env with
		| [] -> raise (Error "no such value")
		| (env1_s,env1_v)::env2 ->
		    if (env1_s = v) then env1_v
		    else getvalue (env2, v)
	    in

	    let rec mx (env, l, v) =
		match l with
		| [] -> v
		| l1::l2 -> 
		    if ((evalu (env, l1)) < (mx (env, l2, (evalu (env, l1))))) then (mx (env, l2, (evalu (env, l1))))
		    else (evalu (env, l1))
	    in

	    match e with
	    | NUM oper -> oper
	    | PLUS (oper1, oper2) -> (evalu (env, oper1)) + (evalu (env, oper2))
	    | MINUS (oper1, oper2) -> (evalu (env, oper1)) - (evalu (env, oper2))
	    | MULT (oper1, oper2) -> (evalu (env, oper1)) * (evalu (env, oper2))
	    | DIVIDE (oper1, oper2) ->
		if ((evalu (env, oper2)) = 0) then raise (Error "DIVIDE 0")
		else ((evalu (env, oper1)) / (evalu (env, oper2)))
	    | MAX oper -> (mx (env, oper, 0))
	    | VAR oper ->
		if (check (env, oper)) then (getvalue (env, oper))
		else raise (Error "no such value")
	    | LET (oper1, oper2, oper3) -> (evalu ((oper1, evalu (env, oper2))::env, oper3))
	in	    

	print_int (evalu (env, e));
	print_string "\n";
	evalu (env, e)
end 
