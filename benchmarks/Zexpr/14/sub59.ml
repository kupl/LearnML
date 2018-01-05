(* 4190,310 Programming Language (Fall 2014)
 * Homework 2 - Exercise 6
 * CSE / 2012-13456 / Gao, Chengbin *)

module type ZEXPR = sig 
    exception Error of string 
    type id = string 
    type expr = NUM of int 
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
    type expr = NUM of int 
              | PLUS of expr * expr 
              | MINUS of expr * expr 
              | MULT of expr * expr 
              | DIVIDE of expr * expr 
              | MAX of expr list 
              | VAR of id 
              | LET of id * expr * expr 

    type environment  = ENV of (id * (environment * expr)) list
    type value = int

    let emptyEnv = ENV []
    let rec eval (env, e) =
        match e with
        | NUM(i) -> i
        | PLUS(e1, e2) -> eval (env, e1) + eval (env, e2)
        | MINUS(e1, e2) -> eval (env, e1) - eval (env, e2)
        | MULT(e1, e2) -> eval (env, e1) * eval (env, e2)
        | DIVIDE(e1, e2) -> eval (env, e1) / eval (env, e2)
        | MAX(lst) -> 
                let vlist = (List.map (fun e0 -> (eval (env, e0))) lst) in
                (match vlist with
                 | [] -> 0
                 | hd::tl -> List.fold_left (max) hd tl)
        | VAR(n) -> 
                (try eval (List.assoc n (match env with ENV l -> l))
                 with Not_found -> raise (Error "FreeVariable"))
        | LET(n, e1, e2) ->
                match env with ENV(lst) ->
                    let new_env = ENV ((n, (env, e1)) :: lst) in
                    eval (new_env, e2)
                

    let int_of_value v = v
end 


(* TODO : test *)

