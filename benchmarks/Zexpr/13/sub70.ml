(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

module Env = Map.Make (String)

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

    type environment = (int) Env.t 
    type value = int
    
    let emptyEnv = Env.empty
    let rec eval (env, e) = 
        match e with
        | NUM(i) -> i
        | PLUS(e1,e2) -> eval(env, e1) + eval(env, e2)
        | MINUS(e1,e2) -> eval(env, e1) - eval(env, e2)
        | MULT(e1,e2) -> eval(env, e1) * eval(env, e2)
        | DIVIDE(e1,e2) -> eval(env, e1) / eval(env, e2)
        | MAX(e_list) -> 
            if (List.length e_list)=0
            then 0 
            else (
                let first_res = eval(env, (List.hd e_list)) in
                List.fold_left(fun res element -> 
                    let element_res = eval(env, element) in
                    if res>=element_res then res
                    else element_res
                ) first_res e_list
            )
        | VAR(id) -> 
            if Env.mem id env
            then Env.find id env
            else raise (Error "FreeVariable")
        | LET(id,e1,e2) ->
            let v = eval(env, e1) in
            let env' = Env.add id v env in
            eval(env', e2)

    let int_of_value v = v
end

