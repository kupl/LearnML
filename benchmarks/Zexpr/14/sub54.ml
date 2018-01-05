module type ZEXPR = 
sig
    exception Error of string
    (* Exception List
     * using not defined variable: "FreeVariable"
     *)
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
    val emptyEnv: environment
    val eval: environment * expr -> value
    val int_of_value : value -> int
end

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
    type value = int
    type environment = (id * value) list
    let (emptyEnv: environment) = []

    let rec max: value * environment * (expr list) -> value =
        fun (v, env, el) ->
            match el with
            | [] -> v
            | hd::tl -> 
                let res = (eval(env, hd)) in
                    if res > v then max(res, env, tl)
                    else max(v, env, tl)

    and eval: environment * expr -> value =
        fun (env, e) ->
            match e with
            | NUM i -> i
            | PLUS (e1, e2) -> (eval (env,e1))+(eval (env,e2))
            | MINUS (e1, e2) -> (eval (env,e1))-(eval (env,e2))
            | MULT (e1, e2) -> (eval (env,e1))*(eval (env,e2))
            | DIVIDE (e1, e2) -> (eval (env,e1))/(eval (env,e2))
            | MAX [] -> 0
            | MAX (hd::tl) -> max((eval (env, hd)), env, tl)
            | VAR x -> (try (List.assoc x env)
                        with Not_found -> raise(Error "FreeVariable"))
            | LET (x, e1, e2) -> eval( (x, (eval(env, e1)))::env, e2 )

    let int_of_value: value -> int =
        fun v ->
            v
end
