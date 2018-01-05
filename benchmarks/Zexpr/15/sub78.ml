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

	type environment = (id * int) list
	type value = int
	
	let emptyEnv = []
	
	let rec eval (env, exp): value =
		match exp with
		| NUM i -> i
		| PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
		| MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
		| MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
		| DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
		| MAX el ->(
			match el with
			| eh::et::el -> if (eval (env, eh)) > (eval (env, et)) then eval (env, MAX (eh::el)) else eval (env, MAX (et::el))
			| eh::[] -> eval (env, eh)
			| [] -> 0 )
		| VAR id ->(
			match env with
			| (st, i)::t -> if st = id then i else eval (t, VAR id)
			| [] -> raise (Error "FreeVariable") )
		| LET (id, e1, e2) -> eval (((id, (eval (env, e1)))::env), e2)
	
	let print_value v :unit = 
		print_int v
end