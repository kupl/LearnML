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
    
    let emptyEnv = []
    let rec eval (env, e) = 
		let rec find_v (enviro, v) = 
			match enviro with
			| [] -> raise (Error "FreeVariable")
			| hd::tl -> (match hd with
							| (a, value) -> if a=v then value else find_v (tl, v)) in

		match e with
		| NUM i -> i
		| PLUS (l, r) -> eval (env, l) + eval (env, r)
		| MINUS (l, r) -> eval (env, l) - eval (env, r)
		| MULT (l, r) -> eval (env, l) * eval (env, r)
		| DIVIDE (l, r) -> eval (env, l) / eval (env, r)
		| MAX li -> (match li with
						| [] -> 0
						| hd::[] -> eval (env, hd)
						| hd::(hd2::tl) -> if eval (env, hd) >= eval (env, hd2) then eval (env, MAX (hd::tl))
										else eval (env, MAX (hd2::tl)))
		| VAR v -> find_v (env, v)
		| LET (id, exp1, exp2) -> eval ((id, eval (env, exp1))::env, exp2)


    let int_of_value v = v
end

