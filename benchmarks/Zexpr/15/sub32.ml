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
	
	type environment = (id * expr) list

	type value = int

	let emptyEnv = []
	
	let rec exp_to_value exp =
		match exp with
		| NUM i -> i
		| PLUS (e1,e2) -> (exp_to_value e1) + (exp_to_value e2)
		| MINUS (e1,e2) -> (exp_to_value e1) - (exp_to_value e2)
		| MULT (e1,e2) -> (exp_to_value e1) * (exp_to_value e2)
		| DIVIDE (e1,e2) -> (exp_to_value e1) / (exp_to_value e2)
		| MAX l ->
			(match l with
			 | [] -> 0
			 | h::[] -> (exp_to_value h)
			 | h::t -> if ((exp_to_value h) > (exp_to_value (MAX t))) then (exp_to_value h)
			 		   else (exp_to_value (MAX t)))
		| VAR str -> raise(Error "FreeVariable")
		| LET (id,e1,e2) -> raise(Error "FreeVariable")
		
		

	let rec eval2 (environ,exp) =
		match (environ,exp) with
		| ([],exp) -> 
			(match exp with
			 | NUM i -> NUM i
			 | PLUS (e1,e2) -> PLUS(eval2([],e1),eval2([],e2))
			 | MINUS (e1,e2) -> MINUS(eval2([],e1),eval2([],e2))
			 | MULT (e1,e2) -> MULT(eval2([],e1),eval2([],e2))
			 | DIVIDE (e1,e2) -> DIVIDE(eval2([],e1),eval2([],e2))
			 | MAX l ->
			 	(match l with
				 | [] -> NUM 0
				 | h::[] -> MAX[eval2([],h)]
				 | h::t -> MAX[eval2([],h);eval2([],MAX t)])
			 | VAR str -> raise(Error "FreeVariable")
			 | LET (id,e1,e2) -> eval2([id,e1],e2))
		| (env::[],exp) ->
			(match exp with
			 | NUM i -> eval2([],NUM i)
			 | PLUS (e1,e2) -> eval2([],PLUS(eval2([env],e1),eval2([env],e2)))
			 | MINUS (e1,e2) -> eval2([],MINUS(eval2([env],e1),eval2([env],e2)))
			 | MULT (e1,e2) -> eval2([],MULT(eval2([env],e1),eval2([env],e2)))
			 | DIVIDE (e1,e2) -> eval2([],DIVIDE(eval2([env],e1),eval2([env],e2)))
			 | MAX l ->
			 	(match l with
				 | [] -> NUM 0
				 | h::[] -> MAX[eval2([env],h)]
				 | h::t -> MAX[eval2([env],h);eval2([env],MAX t)])
			 | VAR str ->
			 	(if (str = (fst env)) then eval2([],(snd env))
				 else eval2([],VAR str))
			 | LET (id,e1,e2) -> eval2([],LET(id,eval2([env],e1),e2)))
		| (env::t,exp) -> eval2(t,eval2([env],exp))

	let eval (env,exp) = 
		exp_to_value(eval2(env,exp))

	let print_value value =
			print_int value

end
