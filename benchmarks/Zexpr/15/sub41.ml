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
	(* Implement this module *)
	exception Error of string
	type id = string
	type value = int
	type environment = (id * value) list
	type expr = 
    | NUM of int 
    | PLUS of expr * expr 
    | MINUS of expr * expr 
    | MULT of expr * expr 
    | DIVIDE of expr * expr 
    | MAX of expr list 
    | VAR of id 
    | LET of id * expr * expr 
	
	let emptyEnv : environment = []
	let print_value v = print_endline(string_of_int v)
	
	let rec find (env, x) =
		match env with
		| [] -> raise (Error "FreeVariable")
		| (h_id, h_val)::t ->
			if(h_id = x) then h_val
			else find (t, x)
	
	let rec reassign (env, x, v) =
		match env with
		| [] -> (x, v)::env
		| (h_id, h_val)::t ->
			if(h_id = x) then (x, v)::t
			else (h_id, h_val)::reassign(t, x, v)
			
	let rec eval (env, exp) =
		match (env, exp) with
		| (_, NUM i) -> i
		| (_, PLUS(e1, e2)) -> eval (env, e1) + eval (env, e2)
		| (_, MINUS(e1, e2)) -> eval (env, e1) - eval (env, e2)
		| (_, MULT(e1, e2)) -> eval (env, e1) * eval (env, e2)
		| (_, DIVIDE(e1, e2)) -> eval (env, e1) / eval (env, e2)
		| (_, MAX el) ->
			(match el with
			| [] -> 0
			| h::[] -> eval (env, h)
			| h::t -> 
				if(eval (env, h) > eval(env, MAX t)) then eval(env, h)
				else (eval (env,MAX t)))
		| (_, VAR x) -> find(env, x) 
		| (en, LET(id1, exp1, exp2)) ->
			begin
			let newEnv = reassign(en, id1, eval(en, exp1)) in
			eval(newEnv, exp2)
			end
	(*
	val emptyEnv : 'a list = []
	val print_value : int -> unit = <fun>
	val find : ('a * 'b) list * 'a -> 'b = <fun>
	val reassign : ('a * 'b) list * 'a * 'b -> ('a * 'b) list = <fun>
	val eval : (id * int) list * expr -> int = <fun>		
	*)	
end
