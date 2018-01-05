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

	let emptyEnv : environment = [] 

	let rec find_match_value: environment * id -> value =
		fun (env, id) ->
			match env with
			| [] -> raise (Error "FreeVariable")
			| head::tail ->
				if((fst head) = id) then snd head
				else find_match_value(tail, id)

	let rec eval : environment * expr -> value = 
		fun (env, expr) ->
			match expr with
			| NUM num -> num
			| PLUS (e1, e2) -> (eval(env, e1)) + (eval(env, e2))
			| MINUS (e1, e2) -> (eval(env, e1)) - (eval(env, e2))
			| MULT (e1, e2) -> (eval(env, e1)) * (eval(env, e2))
			| DIVIDE (e1, e2) -> (eval(env, e1)) / (eval(env, e2))
			| MAX lst -> find_max_value(env, lst)
			| VAR id -> find_match_value(env, id)
			| LET (id, e1, e2) -> (eval ((id, (eval(env, e1)))::env, e2))
	and find_max_value (env, lst) =
		match lst with
		| [] -> 0
		| head::[] -> eval(env, head)
		| head::tail -> 
			let behind_max = find_max_value(env, tail) in
			let evaluated = eval(env, head) in
			if(behind_max > evaluated) then behind_max
			else evaluated
			
	let print_value : value -> unit = 
		fun value -> print_int value
end

