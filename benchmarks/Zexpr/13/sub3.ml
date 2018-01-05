(*
 * Programming Languages, 2013 Fall.
 * HW Code for Exercise 2-4
 * Department of Computer Science and Engineering
 * 2006-11855, Jung Yonghyuk (ever103@snu.ac.kr)
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

    type environment = (id * int) list
    type value = int
    
    let emptyEnv = []
    let rec eval (env, e) =
		match e with
		| NUM i -> i
		| PLUS (e1, e2) -> (eval (env, e1) + eval (env, e2))
		| MINUS (e1, e2) -> (eval (env, e1) - eval (env, e2))
		| MULT (e1, e2) -> (eval (env, e1) * eval (env, e2))
		| DIVIDE (e1, e2) -> (eval (env, e1) / eval (env, e2))
		| MAX el ->
			if (List.length el) = 0 then 0
			else
				List.hd
					(List.sort
						(fun a b -> b - a)
						(List.map (fun x -> eval (env, x)) el)
					)
        | VAR id ->
			(try
				(List.assoc id env)
			with Not_found -> raise (Error "FreeVariable"))
        | LET (id, e1, e2) ->
			let v = eval (env, e1) in
			let new_env = (id, v)::env in
			(eval (new_env, e2))
    let int_of_value v = v
end
