(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

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
    
    let emptyEnv : environment = []
    let rec eval : environment * expr -> value = 
		let rec add : environment * id * value -> environment  = 
			fun(env,id,value) -> match env with
			| [] -> (id, value)::[]
			| (head_id,head_val)::rest ->
				if (compare head_id id) == 0 then
					(head_id,value)::rest
				else
					(head_id,head_val)::add(rest,id,value)
		in
		let rec search : environment * id -> value = 
			fun(search_env, id) -> match search_env with
			| [] -> raise (Error "FreeVariable")
			| (head_id,head_val)::rest ->
				if (compare head_id id) == 0 then
					head_val
				else
					search(rest, id)
		in				
		fun(env, e) -> match e with
        | NUM integer 			-> integer
        | PLUS (expr1,expr2) 	-> eval(env,expr1) + eval(env,expr2)
        | MINUS (expr1,expr2) 	-> eval(env,expr1) - eval(env,expr2)
        | MULT (expr1,expr2) 	-> eval(env,expr1) * eval(env,expr2)
        | DIVIDE (expr1,expr2) 	-> eval(env,expr1) / eval(env,expr2)
        | MAX exprlist 			-> (
			match exprlist with
			| [] -> 0
			| head_expr::[] -> eval(env,head_expr)
			| head_expr::rest -> max (eval(env,head_expr)) (eval(env,(MAX rest)))
		)
        | VAR id -> search(env, id)
        | LET (id, expr1, expr2) -> eval(add(env,id,(eval(env,expr1))),expr2)
    let int_of_value v = v
end

