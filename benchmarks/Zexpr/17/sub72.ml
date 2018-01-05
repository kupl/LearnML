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

	val emptyEnv: environment 
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

	let rec findId (id: id) (env: environment): expr =
		match env with
		| [] -> raise (Error "FreeVariable")
		| (i, e)::t -> if (i=id) then e
						else (findId id t)
(*	
	let rec insertEnv ((id: id), (exp: expr)) (env: environment): environment =
		match env with
		| [] -> [(id, exp)]
		| (i, e)::t -> if (i=id) then env
						else (insertEnv (id, exp) t)
*)
	let rec eval ((env: environment), (exp: expr)): value = 
		match exp with
		| NUM n -> n
		| PLUS (n1, n2) -> (eval (env, n1)) + (eval (env, n2))
		| MINUS (n1, n2) -> (eval (env, n1)) - (eval (env, n2))
		| MULT (n1, n2) -> (eval (env, n1)) * (eval (env, n2))
		| DIVIDE (n1, n2) -> (eval (env, n1)) / (eval (env, n2))
		| VAR i -> (eval (env, (findId i env)))
		| LET (i, ex1, ex2) -> (eval (((i, ex1)::env), ex2))
		| MAX expList -> 
			match expList with 
			| [] -> 0
			| h::t -> (eval (env, (findMax env h t)))
	and findMax (env: environment) (max: expr) (l: expr list): expr =
		match l with
		| [] -> max
		| h::t -> 
			if ((eval (env, max)) < (eval (env, h))) then (findMax env h t)
			else (findMax env max t)
	

	let print_value (v: value) =
		(Printf.printf "%d\n" v)
 
end 

