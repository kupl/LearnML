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

    let rec env_finder (env, v) =
        match env with
        | [] -> raise (Error "FreeVariable")
        | ((var, n)::tl) -> 
                if v = var then n
                else env_finder (tl, v)

    let rec eval (env, e) = 
        let rec max_finder (env, h, l) =
            match l with
            | [] -> eval (env, h)
            | (hd::tl) ->
                    if (eval (env, h)) >= (eval (env, hd)) then max_finder (env, h, tl)
                    else max_finder (env, hd, tl)
        in
        match e with
        | NUM n -> n
        | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
        | MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
        | MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
        | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
        | MAX l -> 
                (match l with
                | [] -> 0
                | (hd::tl) -> max_finder (env, hd, tl))
        | VAR v -> env_finder (env, v)
        | LET (v, e1, e2) -> eval (((v, eval (env, e1))::env), e2)

    let int_of_value v = v
end

