(*2009-11718 2-5*)
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
	type environment = (id * value) list
    
    
    let emptyEnv = []

	let eval (env, e) =
		let rec calc (env, exp) =
			let rec findMax x ex = max x (calc (env, ex)) in 	

			match exp with
			| NUM n -> n
			| PLUS (e1, e2) -> (calc (env, e1))+(calc (env, e2))
			| MINUS (e1, e2) -> (calc (env, e1))-(calc (env, e2))
			| MULT (e1, e2) -> (calc (env, e1))*(calc (env, e2))
			| DIVIDE (e1, e2) -> (calc (env, e1))/(calc (env, e2))
(*			| MAX [] -> 0
			| MAX (hd::tl) -> List.fold_left (fun m x lst -> max (calc (lst.hd)) m) (calc hd) hd::tl
*)
			| MAX l -> (match l with
					| [] -> 0
					| hd::tl -> (List.fold_left findMax min_int l)) 
			| VAR id -> List.assoc id env
			| LET (x, value, ex) -> calc ([(x, calc(env, value))]@env, ex)
		in

	print_int (calc (env, e));
	calc (env, e)
 
end

    
