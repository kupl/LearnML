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

        type value
        type environment

        val emptyEnv : environment
        val eval : environment * expr -> value

        val print_value : value -> unit
    end

module Zexpr : ZEXPR =
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

        type value = int
        type environment = (id * value) list

        let emptyEnv : environment = []

        let rec eval ((env : environment), (e : expr)) : value =
            match (env, e) with
            | (_, NUM(i)) -> i
            | (_, PLUS(e1, e2)) -> eval(env, e1) + eval(env, e2)
            | (_, MINUS(e1, e2)) -> eval(env, e1) - eval(env, e2)
            | (_, MULT(e1, e2)) -> eval(env, e1) * eval(env, e2)
            | (_, DIVIDE(e1, e2)) -> eval(env, e1) / eval(env, e2)
            | (_, MAX(l)) -> let rec list_max l =
                                 match l with
                                 | [] -> 0
                                 | [e1] -> eval(env, e1)
                                 | e1::lm -> let a = eval(env, e1) and b     = list_max(lm) in
                                             if a > b then a else b in
                             list_max l
            | (_, VAR(i)) -> if (List.mem_assoc i env) = false then raise     (Error "FreeVariable")
                             else List.assoc i env
            | (_, LET(i, e1, e2)) -> eval((i, eval(env, e1))::env, e2)

        let print_value (v : value) : unit =
            print_endline (string_of_int v)
    end