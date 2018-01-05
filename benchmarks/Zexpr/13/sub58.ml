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

    type environment = (string * int) list
    type value = int
    
    let emptyEnv = []
    let rec eval (env, e) =
	  match e with
	  | NUM a -> a
	  | PLUS(e1, e2) -> (eval (env, e1))+(eval (env, e2))
	  | MINUS(e1, e2) -> (eval (env, e1))-(eval (env, e2))
	  | MULT(e1, e2) -> (eval (env, e1))*(eval (env, e2))
	  | DIVIDE(e1, e2) -> (eval (env, e1))/(eval (env, e2))
	  | MAX lst -> if (List.length lst)=0 then 0
	  			   else 
				     let hd_val = (eval (env, (List.hd lst))) in
					 let tl_val = (eval (env, (MAX (List. tl lst)))) in
					   if hd_val>tl_val then hd_val
					   else tl_val
	  | VAR str -> if (List.length env)=0 then raise (Error "FreeVariable")
	  			   else if (fst (List.hd env))=str then (snd (List.hd env))
				   else (eval ((List.tl env), e))
	  | LET(str, e1, e2) -> let e1_val = (eval (env, e1)) in
	  						  (eval ((List.append [(str, e1_val)] env), e2))

    let int_of_value v = v
end

