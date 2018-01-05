module type ZEXPR =
	sig
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

		val print_value : value -> unit
	end

module Zexpr : ZEXPR =
	struct
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

		type environment = (string * int) list
		type value = int
  
		let emptyEnv : environment = []
		
		let rec eval (env, exp) = match exp with
			| NUM n -> n
			| PLUS (exp1, exp2) -> 	 eval (env, exp1) + eval (env, exp2) 
			| MINUS (exp1, exp2) ->	 eval (env, exp1) - eval (env, exp2)
			| MULT (exp1, exp2) ->	 eval (env, exp1) * eval (env, exp2)
			| DIVIDE (exp1, exp2) -> eval (env, exp1) / eval (env, exp2)
			| MAX exp_ ->
			(
				match exp_ with
				| [] -> 0
				| x :: [] -> eval (env, x)
				| hd :: tl -> if (eval (env, hd)) > (eval (env, MAX tl)) then (eval (env, hd)) else (eval (env, MAX tl))
			)
			| VAR id -> 
			(
				try let (_, s) = (List.find (fun (v, _ ) -> id = v) env) in s with
				| Not_found -> raise (Error "FreeVariable")
			)
			| LET (id, exp1, exp2) -> let value = eval (env, exp1) in let env_ = (id, value) :: env in eval (env_, exp2)

		let print_value = print_int
	end 
