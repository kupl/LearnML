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

    type environment = ( string * float ) list
    type value = float
    
    let emptyEnv = []
    let rec eval (env, expr) =
		match expr with
		| NUM n -> float_of_int n
		| PLUS (a, b) -> (eval (env, a)) +. (eval (env, b))
		| MINUS (a, b) -> (eval (env, a)) -. (eval (env, b))
		| MULT (a, b) -> (eval (env, a)) *. (eval (env, b))
		| DIVIDE (a, b) ->
			if eval (env, b) = 0.0
			then raise (Error "DividedByZero")
			else (eval (env, a)) /. (eval (env, b))
		| MAX lst ->
			if lst = []
			then 0.
			else
				let rec find elem lst =
					if lst = []
					then elem
					else
						let telem = eval (env, List.hd lst) in
						let tlst = List.tl lst in
						if elem < telem
						then find telem tlst
						else find elem tlst
				in
				find (eval (env, List.hd lst)) (List.tl lst)
		| VAR id ->
			let rec find id env =
				if env = []
				then raise (Error "UndeclaredID")
				else
					let tp = List.hd env in
					if id = fst tp
					then snd tp
					else find id (List.tl env)
			in
			find id env
		| LET (id, expr1, expr2) ->
			eval ((id, eval (env, expr1))::env, expr2)
end

    
