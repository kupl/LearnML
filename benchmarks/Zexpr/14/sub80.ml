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

module Zexpr =
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
   	type environment = (id * value) list 

   	let emptyEnv = []
		let rec max = function 
			| [] -> 0 
			| (e::l) -> 
				if l = [] then e 
				else 
					let v1 = e in 
					let v2 = max(l) in 
					if v1 > v2 then v1 else v2;;
 
		let rec eval = function (*: environment * expr -> value*)
			| (env, e) ->
				begin match e with
				| NUM i -> i
				| PLUS(e1, e2) -> eval(env, e1) + eval(env, e2)
				| MINUS(e1, e2) -> eval(env, e1) - eval(env, e2)
				| MULT(e1, e2) -> eval(env, e1) * eval(env, e2)
				| DIVIDE(e1, e2) -> eval(env, e1) / eval(env, e2)
				| MAX elist -> max(List.map (fun x -> eval(env,x)) elist)
				| VAR id ->
					if (List.mem_assoc id env) then (List.assoc id env)
					else raise (Error "FreeVariable")
				| LET (id, e1, e2) ->
					let v = eval(env, e1) in
						if (List.mem_assoc id env)
						then eval((id,v)::(List.remove_assoc id env),e2)
						else eval((id,v)::env,e2)
				end;;
					 
   	let int_of_value i = i;
  end