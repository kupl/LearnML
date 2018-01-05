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

	type value = VALUE of int
  type environment = (id * value) list
	
	let emptyEnv : environment = []
	
	let int_of_value : value -> int =
		function VALUE n -> n
	
  let rec eval ((env, exp) : environment * expr) : value =
		match exp with
		| NUM n -> VALUE n
		| PLUS (e1, e2) -> VALUE ((int_of_value (eval (env, e1))) + (int_of_value (eval (env, e2))))
		| MINUS (e1, e2) -> VALUE ((int_of_value (eval (env, e1))) - (int_of_value (eval (env, e2))))
		| MULT (e1, e2) -> VALUE ((int_of_value (eval (env, e1))) * (int_of_value (eval (env, e2))))
		| DIVIDE (e1, e2) -> VALUE ((int_of_value (eval (env, e1))) / (int_of_value (eval (env, e2))))
		| MAX [] -> VALUE 0
		| MAX [e] -> eval (env, e)
		| MAX (h::r) ->
			let v1 = int_of_value (eval (env, h))
			and v2 = int_of_value (eval (env, MAX r)) in
			VALUE (max v1 v2)
		| VAR x ->
			if List.mem_assoc x env
				then List.assoc x env
				else raise (Error "FreeVariable")
		| LET (x, e1, e2) ->
			let xval = eval (env, e1) in
			eval ((x, xval)::env, e2)

end
