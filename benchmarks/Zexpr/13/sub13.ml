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

    type environment = (id * int) list
    type value = int
    
    let emptyEnv = []
    let rec eval (env, e) =
	let rec max lst cur =
		match lst with
		| [] -> cur
		| hd::tl -> if (eval (env, hd))>cur then max tl (eval (env, hd))
			    else max tl cur
	in
	let rec contains (env, x) =
		match env with
		| [] -> (false, 0)
		| (a, b)::tl -> if a=x then (true, b)
				else contains (tl, x)
	in
	let rec overwrite (env, x, y) =
		match env with
		| [] -> []
		| (a, b)::tl -> if a=x then (a, y)::tl
				else (a, b)::(overwrite (tl, x, y))
	in
	match e with
	| NUM x -> x
	| PLUS (x, y) -> eval(env, x) + eval(env, y)
	| MINUS (x, y) -> eval(env, x) - eval(env, y)
	| MULT (x, y) -> eval(env, x) * eval(env, y)
	| DIVIDE (x, y) -> eval(env, x) / eval(env, y)
	| MAX lst -> (match lst with 
			| [] -> 0
			| hd::tl -> max tl (eval (env, hd))
		     )
	| VAR x ->( match contains (env, x) with
		| (true, a) -> a
		| (false, _) -> raise (Error "FreeVariable")
		)
	| LET (x, y, z) -> (match contains (env, x) with
			| (true, _) -> eval(overwrite(env, x, eval(env, y)), z)
			| (false, _) -> eval((x, eval(env, y))::env, z)
			)


    let int_of_value v = v
end

