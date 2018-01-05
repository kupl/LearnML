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
    
    type value = int
    type environment = (id * int) list 
    let emptyEnv = []
    let int_of_value = fun (x) -> x

    let rec eval = fun (env, e) ->
        match e with
        | NUM(n) -> n
        | PLUS(e1,e2) -> eval(env,e1) + eval(env,e2)
        | MINUS(e1,e2) -> eval(env,e1) - eval(env,e2)
        | MULT(e1,e2) -> eval(env,e1) * eval(env,e2)
        | DIVIDE(e1,e2) -> let x = eval(env,e2) in if x = 0 then raise(Error "Divide by zero") else eval(env,e1) / x
        | VAR(v) -> assign(env,v)
        | LET(v,e1,e2) -> eval((v, eval(env,e1))::env, e2)
        | MAX(el) ->
            match el with
            | [] -> 0
            | e::[] -> eval(env,e)
            | e::tl -> let x = eval(env,e) in
                       let y = eval(env,MAX(tl)) in
                           if x < y then y else x

    and assign = fun (env, x) ->
        match env with
        | [] -> raise(Error "Free variable")
        | (id,value)::tl -> if id = x then value else assign(tl, x)

end

