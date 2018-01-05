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
	
	type value = int
	type environment = (id * value) list
    let emptyEnv = []
	let int_of_value value = value
	
    let rec eval (env, expr) = 
		match expr with
        | NUM i -> i
        | PLUS (e1, e2) -> eval(env,e1) + eval(env,e2)
        | MINUS (e1, e2) -> eval(env,e1) - eval(env,e2)
        | MULT (e1, e2) -> eval(env,e1) * eval(env,e2)
        | DIVIDE (e1, e2) -> eval(env,e1) / eval(env,e2)
        | MAX [] -> 0 
		| MAX (fst::[]) -> eval(env,fst)
		| MAX (fst::snd::rest) -> 
			let v1 = eval(env,fst) in
			let v2 = eval(env,snd) in
			eval(env,MAX(NUM(max v1 v2)::rest))
        | VAR str -> 
			let rec findValue env = 
				match env with
				| [] -> raise (Error "FreeVariable")
				| (id, value)::rest ->
					if id=str then value
					else findValue rest
			in
			findValue env		
        | LET (str, value, exp) -> 
			let newVar = (str,eval(env,value)) in
			eval(newVar::env,exp)
end
