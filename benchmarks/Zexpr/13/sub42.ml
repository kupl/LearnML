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
        | NUM x -> x
        | PLUS (x, y) -> eval(env, x) + eval(env, y)
        | MINUS (x, y) -> eval(env, x) - eval(env, y)
        | MULT (x, y) -> eval(env, x) * eval(env, y)
        | DIVIDE (x, y) -> eval(env, x) / eval(env, y)
        | MAX [] -> 0
        | MAX l -> (List.hd (List.sort (fun x y -> compare y x) (List.map (fun x -> eval(env, x)) l)))
        | VAR v -> (try (List.assoc v env) with Not_found -> raise (Error "FreeVariable"))
        | LET (id, e1, e2) -> eval((id, eval(env, e1))::env, e2)

    let int_of_value v = v
end

