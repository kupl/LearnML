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
    type environment = (id, value) Hashtbl.t

    let value_of_int i = i
    let int_of_value v = v
    
    let emptyEnv = Hashtbl.create 10
    let rec eval (env, e) = 
        match e with
        | NUM d -> value_of_int d
        | PLUS (e1, e2) -> value_of_int ((int_of_value (eval (env,e1))) + (int_of_value (eval (env,e2))))
        | MINUS (e1, e2) -> value_of_int ((int_of_value (eval (env,e1))) - (int_of_value (eval (env,e2))))
        | MULT (e1, e2) -> value_of_int ((int_of_value (eval (env,e1))) * (int_of_value (eval (env,e2))))
        | DIVIDE (e1, e2) -> value_of_int ((int_of_value (eval (env,e1))) / (int_of_value (eval (env,e2))))
        | MAX l -> if l=[] then value_of_int 0
                   else let max_internal environ exp =
                            int_of_value (eval (environ, exp))
                        in
                        let compare_reverse a b =
                            -1 * compare a b
                        in value_of_int (List.hd (List.sort compare_reverse (List.map (max_internal env) l)))
        | VAR v -> (try
            (Hashtbl.find env v)
                with Not_found -> raise (Error "FreeVariable"))
        | LET (id, e1, e2) -> let cur_env = (Hashtbl.copy env)
                               in Hashtbl.add cur_env id (eval (env, e1));
                               eval (cur_env, e2)
end

