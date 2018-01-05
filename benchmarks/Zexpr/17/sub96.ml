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
		val emptyEnv : environment
		val eval : environment * expr -> value
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
		type environment = (string * int) list
		type value = int
		let emptyEnv = []

		let rec findEnv env str =
			match env with
			| [] -> 0
			| h::t -> 
				match h with
				| (a,b) -> if (compare a str) == 0 then b
					else findEnv t str
		let rec eval = function
			| (env, expr) ->
				match expr with
				| NUM(a) -> a
				| PLUS (a,b) -> eval(env, a) + eval(env, b)
				| MINUS (a,b) -> eval(env, a) - eval(env, b)
				| MULT (a,b) -> eval(env, a) * eval(env, b)
				| DIVIDE (a,b) -> eval(env, a) / eval(env, b)
				| MAX ([]) -> 0
				| MAX (h :: t) -> (
						let rec calculmax max one =
							match max with
							| [] -> one
							| h :: t -> if eval(env, h) > one then calculmax t (eval(env, h))
								else calculmax t one in
						calculmax t (eval(env, h))) 
				| LET (a, b, c) ->
					eval ((a, eval(env, b)) :: env, c)
				| VAR (a) -> findEnv env a
		let print_value a = print_int a
	end					

