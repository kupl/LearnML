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
    		| PLUS(e1, e2) -> eval(env, e1) + eval(env, e2)
    		| MINUS(e1, e2) -> eval(env, e1) - eval(env, e2)
    		| MULT(e1, e2) -> eval(env, e1) * eval(env, e2)
    		| DIVIDE(e1, e2) -> eval(env, e1) / eval(env, e2)
    		| MAX(el) -> (match el with
    										[] -> 0
    										| h::t -> if t = [] then eval (env, h)
    																				else (let current = eval (env, h)
    																								in let next = eval (env, MAX t)
    																									in (if current > next then current else next))
    									)
    		| VAR(v) -> (match env with
    									[] -> raise (Error ("No value for the variable " ^ v ^ " in this env.\n"))
    									| (hi,hv)::t -> if hi=v then hv else eval(t, e))
    		| LET(v, e1, e2) -> if List.mem_assoc v env then eval((v, eval(env, e1))::(List.remove_assoc v env), e2)
    																								else eval((v, eval(env, e1))::env, e2)

    let int_of_value v = v
end
