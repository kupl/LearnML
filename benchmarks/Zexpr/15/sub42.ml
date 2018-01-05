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
	val print_value : value -> unit 
	(* val int_of_value : value -> int *)
end 

module Zexpr : ZEXPR = struct 
	exception Error of string (* "FreeVariable" *)
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

	let emptyEnv: environment = []

	let rec find ((env: environment), (m: id)): value = 
		match env with 
		| [] -> raise (Error "FreeVariable")
		| h::t -> match h with
				  | (i, v) -> if (i = m) then v
					          else (find(t, m))

	let rec eval: environment * expr -> value =
	fun (env, ex) -> 
		match ex with
		| NUM i -> i
		| PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
		| MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
		| MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
		| DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
		| MAX el -> (match el with
					| [] -> 0
					| h::t -> match t with
							  | [] -> eval (env, h)
							  | h2::t2 -> if eval(env, h) > eval(env, h2) then eval(env, MAX([h]@t2))
							  			  else eval(env, MAX(t)) )
		| VAR i -> find (env, i)
		| LET (i, e1, e2) -> eval([(i, eval(env, e1))]@env, e2)

	(* let int_of_value (v: value): int = v *)

	let print_value: value -> unit =
	fun v ->
		print_int (v);

end 