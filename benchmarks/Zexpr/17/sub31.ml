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

  type environment = (id * value) list

  let emptyEnv = []
  let rec eval : environment * expr -> value = fun (env,exp) ->
  	match exp with
	| NUM i -> i
	| PLUS (e1,e2) -> (eval (env,e1)) + (eval (env,e2))	
	| MINUS (e1,e2) -> (eval (env,e1)) - (eval (env,e2))
	| MULT (e1,e2) -> (eval (env,e1)) * (eval (env,e2))
	| DIVIDE (e1,e2) -> (eval (env,e1)) / (eval (env,e2))
	| MAX l -> (
				let rec max_helper : (value * expr list) -> value = fun p -> 
					match p with
					| (v,[]) -> v
					| (v,hd::tl) -> max_helper ((max v (eval(env,hd))), tl)
				in
				match l with
				| [] -> 0
				| hd::tl -> max_helper ((eval(env,hd)),tl)
				)
	| VAR id -> (try (List.assoc id env) with Not_found -> (raise (Error "FreeVariable")))
	| LET (id,e1,e2) -> eval (((id,(eval(env,e1)))::(List.remove_assoc id env)),e2)

  let print_value : value -> unit = fun v ->
  	print_endline (string_of_int v)

end 
