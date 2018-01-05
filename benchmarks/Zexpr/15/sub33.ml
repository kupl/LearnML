(* C:\Users\saigoy\Desktop\Zexpr.ml *)

module type ZEXPR = 
  sig
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
  	
  	type environment
  	
	type value
  	
	val emptyEnv : environment
  	
	val eval : environment * expr -> value
  
	val print_value : value -> unit
  
  end;;
  
  module Zexpr : ZEXPR = 
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
	
	type environment = (id * int) list
	type value = int
	
	let emptyEnv = []
	
	let rec eval (env, expr) =
	match expr with
	| NUM n -> n
	| PLUS (le, re) -> (eval (env, le)) + (eval (env, re))
	| MINUS(le, re) -> (eval (env, le)) - (eval (env, re))
	| MULT(le, re) -> (eval (env, le)) * (eval (env, re))
	| DIVIDE(le, re) -> (eval (env, le)) / (eval (env, re))
	| MAX elst -> 
	(
		let vlst = List.map ( fun x -> eval (env, x) ) elst in
		match vlst with
		| [] -> 0
		| hd::tl -> List.fold_left max hd tl
	)
	| VAR id -> 
	(
		try (snd (List.find (fun x -> (String.compare (fst x) id) == 0) env)) 
		with ex -> raise (Error "FreeVariable")
	)
	| LET (id, le, re) -> 
	(
		let v = (eval (env, le)) in
		let newEnv = ((id, v)::env) in
		eval (newEnv, re)
	)

	let print_value v = (print_string (string_of_int (v)))
	
		
  end;;