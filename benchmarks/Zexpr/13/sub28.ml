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
	match e with 
		NUM(i) -> i
		| PLUS(e1, e2) -> ((eval (env, e1)) + (eval (env, e2)))
		| MINUS(e1, e2) -> ((eval (env, e1)) - (eval (env, e2)))
		| MULT(e1, e2) -> ((eval (env, e1)) * (eval (env, e2)))
		| DIVIDE(e1, e2) -> ((eval (env, e1)) / (eval (env, e2)))
		| MAX(el) -> (
			if (el = []) then 0
			else (List.fold_left (max) (List.hd (List.map (fun x -> (eval (env, x))) el)) (List.tl (List.map (fun x -> (eval (env, x))) el))))
			
		| VAR(id) -> (
			match env with 
				[] -> raise (Error "FreeVariable")
				| (x, y)::tl -> (if (x = id) then y
						 else (eval (tl, (VAR(id)))) ) )
		| LET(id, e1, e2) -> (eval (((id, (eval (env, e1)))::env), e2))
				 	

    let int_of_value v = v
end

