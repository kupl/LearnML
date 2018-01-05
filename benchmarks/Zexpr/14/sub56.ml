module type ZEXPR =
	sig
		exception Error of string
		type id = string
		type expr = NUM of int
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
		val int_of_value: value -> int
	end

module Zexpr =
	struct
		exception Error of string
		type id = string
		type expr = NUM of int
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
		let int_of_value = function x -> x
		let rec eval = function (_, NUM i) -> i
		                      | (env, PLUS (e1, e2)) -> (eval (env, e1)) + (eval (env, e2))
							  | (env, MINUS (e1, e2)) -> (eval (env, e1)) - (eval (env,e2))
							  | (env, MULT (e1, e2)) -> (eval (env, e1)) * (eval (env, e2))
							  | (env, DIVIDE (e1, e2)) -> (eval (env, e1)) / (eval (env, e2))
							  | (env, MAX []) -> 0
							  | (env, MAX (e::elist)) -> (match elist with [] -> (eval (env, e))
							                                           | _ -> let a1 = (eval (env, e)) in let a2 = (eval (env, MAX elist)) in if a1 > a2 then a1 else a2)
                              | (env, VAR x) -> if (List.mem_assoc x env) then (List.assoc x env) else raise (Error "FreeVariable")
							  | (env, LET (x, e1, e2)) -> let xval = (eval (env, e1)) in
							                              let newenv = (x, xval)::env in
														  (eval (newenv, e2))
end	
