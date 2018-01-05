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

    type environment = (id * float) list
    type value = float
    
    let emptyEnv = []
    let rec eval (env, e) = 
	match e with
	| NUM i -> float_of_int i
	| PLUS (e1, e2) -> (eval (env, e1)) +. (eval (env, e2))
	| MINUS (e1, e2) -> (eval (env, e1)) -. (eval (env, e2))
	| MULT (e1, e2) -> (eval (env, e1)) *. (eval (env, e2))
	| DIVIDE (e1, e2) -> (eval (env, e1)) /. (eval (env, e2))
	| MAX list ->
		(match list with
		| [] -> 0.0
		| head::tail -> (max (eval (env, head)) (eval (env, MAX tail)))
		)
	| VAR id ->
		if List.length env < 1 then raise (Error "FreeVariable")
		else
			if List.exists (fun x -> (fst x) = id) env
			then (snd (List.find (fun x -> (fst x) = id) env))
			else raise (Error "FreeVariable")
	| LET (id, e1, e2) ->
		eval ((id, (eval (env, e1)))::env, e2)
		
	

    let int_of_value v = int_of_float v
end

