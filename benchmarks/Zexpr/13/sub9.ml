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
    let eval (env, e) = 
		let rec find_max : environment * expr list -> int = fun (env, lst) ->
			let rec find_max_sub (env, lst, max_val) =
				match lst with
				| [] -> max_val
				| h::t -> 
					if eval_sub (env, h) < max_val then find_max_sub (env, t, max_val)
					else find_max_sub (env, t, (eval_sub (env, h)))
			in
			find_max_sub (env, lst, 0)
		and

		val_in_env : environment * id -> int = fun (env, id) ->
			match env with
			| [] -> raise (Error "FreeVariable")
			| h::t -> 
				if (fst h) = id then (snd h)
				else val_in_env (t, id)
		and

		eval_sub (env, e) =
			match e with
			| NUM n -> n
			| PLUS (e1, e2) -> eval_sub (env, e1) + eval_sub (env, e2)
			| MINUS (e1, e2) -> eval_sub (env, e1) - eval_sub (env, e2)
			| MULT (e1, e2) -> eval_sub (env, e1) * eval_sub (env, e2)
			| DIVIDE (e1, e2) -> eval_sub (env, e1) / eval_sub (env, e2)
			| MAX lst -> find_max (env, lst)
			| VAR x -> val_in_env (env, x)
			| LET (id, e1, e2) -> eval_sub ((id, eval_sub (env, e1))::env, e2)
		in

		eval_sub (env, e)

    let int_of_value v = v
end

