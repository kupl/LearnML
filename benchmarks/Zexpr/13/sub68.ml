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

    type environment = (id * expr) list
    type value = int

    let emptyEnv = []
    let rec eval (env, e) =
		match e with
		| NUM n -> n
		| PLUS(e1,e2) -> eval(env,e1) + eval(env,e2)
		| MINUS(e1,e2) -> eval(env,e1) - eval(env,e2)
		| MULT(e1,e2) -> eval(env,e1) * eval(env,e2)
		| DIVIDE(e1,e2) -> eval(env,e1) / eval(env,e2)
		| MAX lst -> if lst=[] then 0
					else if List.tl lst =[] then eval(env,List.hd lst)
					else if eval(env,List.hd lst) > eval(env,MAX (List.tl lst)) then eval(env,List.hd lst)
					else eval(env,MAX (List.tl lst))	
		| VAR x -> let rec expr_of_x (envin, varin) = 
						match envin with
						| [] -> raise (Error "FreeVariable")
						| hd::tl -> if (fst hd) = varin then (snd hd)
									else expr_of_x(tl,varin) in
					eval(env,expr_of_x(env,x))	
		| LET(x,e1,e2) -> eval((x,e1)::env,e2)

    let int_of_value v = v
end

