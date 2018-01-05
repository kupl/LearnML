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
    let emptyEnv = []
    let rec eval (env, exp) =
    match exp with
    | LET (id,ex1,ex2) -> eval ([(id,ex1)]@env, ex2) 
    | NUM a -> a
    | PLUS (ex1,ex2) -> eval (env,ex1) + eval (env,ex2)
    | MINUS (ex1,ex2) -> eval (env,ex1) - eval (env, ex2)
    | MULT (ex1,ex2) -> eval (env,ex1) * eval (env, ex2)
    | DIVIDE (ex1,ex2) -> eval (env,ex1) / eval (env, ex2)
    | VAR id -> if (List.mem_assoc id env)&&(fst(List.hd(env))=id) then eval(List.tl(env),snd(List.hd(env)))
    	else if (List.mem_assoc id env&&(fst(List.hd(env))!=id)) then eval(List.tl(env), VAR id)
    	else raise (Error "FreeVariable")
    | MAX a ->
    	match a with
    	| [] -> 0
        | a::[] -> eval (env,a)
    	| a::b -> if eval (env, a) > eval (env, MAX b) then eval (env, a)
    	else eval (env, MAX b)
    let print_value a = print_int(a) ; print_newline ()
end




