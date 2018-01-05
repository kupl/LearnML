module type ZEXPR = 
sig
	exception Error of string
	type id = string
	type expr = NUM of int
		|PLUS of expr * expr
		|MINUS of expr * expr
		|MULT of expr * expr
		|DIVIDE of expr * expr
		|MAX of expr list
		|VAR of id
		|LET of id * expr * expr
	type environment
	type value
	val emptyEnv: environment
	val eval: environment * expr -> value
end

module Zexpr : ZEXPR =
struct
	exception Error of string
	type id = string
	type expr = NUM of int
		|PLUS of expr * expr
		|MINUS of expr * expr
		|MULT of expr * expr
		|DIVIDE of expr * expr
		|MAX of expr list
		|VAR of id
		|LET of id * expr * expr
	type environment = (id * expr) list
	type value = int
	let emptyEnv =  []
	let eval (env,e) = 
		let rec calc : environment * expr -> value = fun(env,expr) ->
			match expr with
			NUM i -> i
			|PLUS(e1,e2) -> calc (env,e1) + calc (env,e2) 
			|MINUS(e1,e2) -> calc (env,e1) - calc (env,e2)
			|MULT(e1,e2) -> calc (env,e1) * calc (env,e2)
			|DIVIDE(e1,e2) -> if calc (env,e2) == 0 then raise (Error "Division by zero")
					  else calc (env,e1) / calc (env,e2)
			|MAX l -> 	
				let f a = calc(env,a) in
				let rec getMax cur l = match l with
					[] -> cur
					|h::t -> if h > cur then getMax h t else getMax cur t in
				(match l with
				[] -> 0
				|_ -> let fl = List.map f l in getMax (List.hd fl) (List.tl fl)
				)
			|VAR id -> 
				(match env with
				[] -> raise (Error "Undefined variable")
				|(x,v)::t -> if (String.compare x id) == 0 then calc(t,v) else calc(t,expr)
				)
			|LET(id,e1,e2) -> calc((id,e1)::env,e2)
		in
		let result = calc(env,e) in print_int result; result
end

