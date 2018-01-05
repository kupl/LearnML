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

	val int_of_value : value -> int
end


module ZEXPR : ZEXPR = 
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
	
	type environment = (id * value) list
	and value = int

	let emptyEnv = []
	
	let isExist (env, name) = 
		match env with
		| [] -> false
		| _ -> List.exists (fun (x,y) -> (x = name)) env

	let getEnv (env, name) = 
		if isExist (env, name) = false 
		then raise (Error "FreeVariable")
		else snd (List.find (fun (x,y) -> (x = name)) env)

	let setEnv (env, name, v) =
		if isExist (env, name) = false
		then (name,v)::env
		else List.map (fun (x,y) -> if (x = name) then (x,v) else (x,y)) env
	
	let getMax l = 
	  	List.fold_left (fun acc x -> max acc x) (List.hd l) l

	let rec eval (env, expr) =
		match expr with
		| NUM n -> n
		| PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
		| MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
		| MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
		| DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
		| MAX [] -> 0
		| MAX expList -> getMax (List.map (fun x -> eval (env, x)) expList) 
		| VAR id -> getEnv(env, id)
		| LET (id,e1,e2) -> eval (setEnv(env,id,eval(env,e1)), e2)
	
	let int_of_value v = v
end
