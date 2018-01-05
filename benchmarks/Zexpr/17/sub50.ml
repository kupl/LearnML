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
	
	type value = int
	
	type env = id * value
	type environment = env list

	let emptyEnv = []
	let rec eval = fun (env, e) ->
		match e with
		| NUM a -> a
		| PLUS (a, b) -> eval (env, a) + eval (env, b)
		| MINUS (a, b) -> eval (env, a) - eval (env, b)
		| MULT (a, b) -> eval (env, a) * eval (env, b)
		| DIVIDE (a, b) -> eval (env, a) / eval (env, b)
		| MAX [] -> 0
		| MAX (h::[]) -> eval(env, h)
		| MAX (h::t) -> if (eval(env, h) > eval(env, MAX t)) then eval(env, h)
							   else eval(env, MAX t)
		| VAR a -> let x = try List.assoc a env with Not_found -> raise (Error "FreeVariable") in x
		| LET (s, a, b) -> eval((s, eval(env, a)) :: env, b)

	let print_value x = print_int x; print_newline()	
end
