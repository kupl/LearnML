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
	
	val emptyEnv: environment
	val eval: environment * expr -> value
	
	val int_of_value : value -> int
end

module Zexpr =
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
	type environment = (id * expr) list
	type value = int
	let rec env_search ((env:environment),(var:id))=
		match env with
		|[] -> raise (Error var)
		|hd::tl->if (String.compare var (fst hd))==0 then (snd hd)
				else (env_search (tl,var))
	let emptyEnv=[]



	let rec eval ((env:environment), (exp:expr))=
		match exp with
		|NUM a -> a
		|PLUS (exp1,exp2) -> (eval (env,exp1))+(eval (env,exp2))
		|MINUS (exp1,exp2) -> (eval (env,exp1))-(eval (env,exp2))
		|MULT (exp1,exp2) -> (eval (env,exp1))*(eval (env,exp2))
		|DIVIDE (exp1,exp2) -> (eval (env,exp1))/(eval (env,exp2))
		|MAX [] -> 0
		|MAX (hd::[])->(eval (env,hd))
		|MAX (hd::tl) -> if (eval (env, hd))<(eval (env, (List.hd tl))) then (eval
				(env,MAX tl))
	else (eval (env, MAX ([hd]@(List.tl tl))))
		|VAR x -> eval (env,(env_search (env,x)))
		|LET (var,e1,e2) -> eval (((var,NUM(eval(env,e1)))::env),e2)
	let int_of_value (v:value)=v
end


