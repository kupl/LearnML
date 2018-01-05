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
      let max ls =
        let rec max_sub ls n =
          match ls with
          | [] -> n
          | hd::tl -> if hd > n then max_sub tl hd else max_sub tl n in
        max_sub (List.tl ls) (List.hd ls) in
      match e with
      | NUM n -> n
      | PLUS (xp1, xp2)   -> eval (env, xp1) + eval (env, xp2)
      | MINUS (xp1, xp2)  -> eval (env, xp1) - eval (env, xp2)
      | MULT (xp1, xp2)   -> eval (env, xp1) * eval (env, xp2)
      | DIVIDE (xp1, xp2) -> eval (env, xp1) / eval (env, xp2)
      | MAX [] -> 0
      | MAX xp_ls -> max (List.map (fun xp -> eval (env, xp)) xp_ls)
      | VAR x ->
         (try List.assoc x env
          with Not_found -> raise (Error "FreeVariable"))
      | LET (x, xp1, xp2) ->
          let new_env = ((x, (eval (env, xp1)))::env) in
          eval (new_env, xp2)

    let int_of_value v = v
end

