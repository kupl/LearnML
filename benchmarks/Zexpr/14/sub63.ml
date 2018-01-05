module type ZEXPR =
    sig
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
    end;;


module Zexpr =
    struct
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
        let rec eval = function
            | (env,NUM n) -> n
            | (env,PLUS(e1,e2)) -> (eval(env,e1))+(eval(env,e2))
            | (env,MINUS(e1,e2)) -> (eval(env,e1))-(eval(env,e2))
            | (env,MULT(e1,e2)) -> (eval(env,e1))*(eval(env,e2))
            | (env,DIVIDE(e1,e2)) -> (eval(env,e1))/(eval(env,e2))
            | (env,MAX []) -> 0
            | (env,MAX (h::[])) -> (eval(env,h))
            | (env,MAX (h::l)) -> (max (eval(env,h)) (eval(env,MAX l)))
            | (env,VAR id) -> (try (List.assoc id env) with
            Not_found -> raise (Error "FreeVariable"))
            | (env,LET(id,e1,e2)) -> eval((id,(eval(env,e1)))::env,e2)
        let int_of_value = fun x -> x
    end;;
