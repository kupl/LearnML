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
	let find_val env id = if List.mem_assoc id env then List.assoc id env else raise (Error "FreeVariable") in
	let update_val env id value = if List.mem_assoc id env then List.map (fun (x, s) -> if x = id then (id, value) else (x, s)) env else (id, value) :: env in
	let eval_max env l = let max = ref 0 in for i = 0 to List.length l - 1 do if !max < (eval (env, (List.nth l i))) then max := (eval (env, (List.nth l i))) done; !max in
        match e with
	| NUM n -> n
	| PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
	| MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
	| MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
	| DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
	| MAX l -> eval_max env l
	| VAR v -> find_val env v
	| LET (id, e1, e2) -> eval (update_val env id (eval (env, e1)), e2)

    let int_of_value v = v
end

