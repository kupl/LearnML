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
    exception FreeVariable
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
    
    let emptyEnv = ([]:environment)
    let rec eval (env, e) =
        match e with
        | NUM i -> i
        | PLUS (e1, e2) -> eval(env, e1) + eval(env, e2)
        | MINUS (e1, e2) -> eval(env, e1) - eval(env, e2)
        | MULT (e1, e2) -> eval(env, e1) * eval(env, e2)
        | DIVIDE (e1, e2) -> eval(env, e1) / eval(env, e2)
        | MAX lst -> max(env, lst)
        | VAR x -> findVar(env, x)
        | LET (x, e1, e2) -> eval(addVar(env, (x, eval(env, e1))), e2)
    and max (env, lst) =
        let rec lambda (lst, max) =
            match lst with
            | [] -> max
            | h::t -> let temp = eval(env, h) in
                        if max < temp then lambda(t,temp) else lambda(t, max)
        in
        lambda(lst, 0)
    and addVar (env, pair) =
        pair::env
    and findVar (env, id) =
        try List.assoc id env with Not_found -> raise FreeVariable

    let int_of_value v = v
end

