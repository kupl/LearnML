(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 ********************************************
 ***edited by student!!!***
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

    type environment = (string*int) list(* TODO *)
    type value = int (* TODO *)
    let emptyEnv = [](* TODO *)
    let rec eval (env, e) = 
		match e with
		| NUM n -> n
        | PLUS(e1,e2) -> eval(env, e1)+eval(env, e2)
        | MINUS(e1,e2) -> eval(env, e1)-eval(env, e2)
        | MULT(e1,e2) -> eval(env, e1)*eval(env, e2)
        | DIVIDE(e1,e2) -> eval(env, e1)/eval(env, e2)
        | MAX(nl) -> if (List.length nl) == 0 then 0
					 else if (List.length nl) == 1 then eval(env,List.hd nl)
					 else (
						 if eval(env,List.hd nl)< eval(env,MAX(List.tl nl)) then eval(env,MAX(List.tl nl))
						 else eval(env,List.hd nl)
					)
        | VAR(id) -> if (List.length env) == 0 then raise(Error "FreeVariable")
					 else(
						 if (compare (fst (List.hd env)) id) == 0 then snd (List.hd env)
						 else eval(List.tl env, VAR(id))
					 )
        | LET(id,e1,e2) -> eval((id,eval(env,e1))::env,e2)

    let int_of_value v =
		v(* TODO *)
end
