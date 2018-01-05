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
    type environment = ENV of (id * value) list
    
    let emptyEnv = ENV []

    let rec eval (env, e) = 
        match e with
        | NUM i -> i
        | PLUS (a, b) -> eval (env, a) + eval (env, b)
        | MINUS (a, b) -> eval (env, a) - eval (env, b)
        | MULT (a, b) -> eval (env, a) * eval (env, b)
        | DIVIDE (a, b) -> eval (env, a) / eval (env, b)
        | MAX l -> (if l = [] then 0 else List.hd (List.rev (List.sort compare (apply_env env l []))))
        | VAR i -> (match env with
                   | ENV l -> if List.exists (fun x -> fst x = i) l
                              then snd (List.find (fun x -> fst x = i) l)
                              else raise (Error "FreeVariable"))
        | LET (i, a, b) -> (match env with
                           | ENV l -> eval (ENV ((List.remove_assoc i l)@[(i, eval (env, a))]), b))

    and apply_env env src dst =
        match src with
        | hd::tl -> apply_env env tl dst@[eval (env, hd)]
        | _ -> dst

    let int_of_value v = v
end

