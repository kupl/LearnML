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
    type def = id * value
    type environment = def list
    
    let emptyEnv = []
    let rec value_in_env var env = 
            match env with
            | [] -> raise (Error "FreeVariable")
            | (id, v)::t ->
              if (id = var) then v
              else (value_in_env var t)
              
    let rec add_env newDef env = 
        match env with
        | [] -> newDef::[]
        | (id, v)::t ->
          if (id = (fst newDef)) then newDef::t
          else (id, v)::(add_env newDef t)

    let rec find_max lst mVal = 
        match lst with
        | [] -> mVal
        | h::t ->
          if h > mVal then (find_max t h)
          else (find_max t mVal)

    let int_of_value v = v
    
    
    let rec eval (env, e) = 
        match e with
        | NUM n -> n
        | PLUS(a, b) -> (eval (env, a)) + (eval (env, b))
        | MINUS(a, b) -> (eval (env, a)) - (eval (env, b))
        | MULT(a, b) -> (eval (env, a)) * (eval (env, b))
        | DIVIDE(a, b) -> (eval (env, a)) / (eval (env, b))
        | MAX l -> 
         (match l with
         | [] -> 0
         | h::t ->
           let newL = (make_eval_list l env) in
           (find_max newL (eval (env, h)))
         )
        | VAR id -> (value_in_env id env)
        | LET (id, ev, exp) ->
          let newDef = (id, (eval (env, ev))) in
          let newEnv = (add_env newDef env) in
          (eval (newEnv, exp))
    and make_eval_list lst env = 
        match lst with
        | [] -> []
        | h::t -> (eval (env, h))::(make_eval_list t env)



end

