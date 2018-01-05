(*HW2-Exercise 7*)
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

    let emptyEnv = []
    let rec eval (env, exp) =
        match exp with
        | NUM n -> n
        | PLUS (exp1, exp2) -> eval(env, exp1) + eval(env, exp2)
        | MINUS (exp1, exp2) -> eval(env, exp1) - eval(env, exp2)
        | MULT (exp1, exp2) -> eval(env, exp1) * eval(env, exp2)
        | DIVIDE (exp1, exp2) -> eval(env, exp1) / eval(env, exp2)
        | MAX explist -> begin
            let rec evalmax (env, explist) max =
                match explist with
                | [] -> max
                | exp::explist_rest -> begin
                    let m = eval(env, exp) in
                    if m > max then evalmax(env, explist_rest) m
                    else evalmax(env, explist_rest) max
                end
            in
            match explist with
            | [] -> 0
            | exp::explist_rest -> evalmax (env, explist_rest) (eval(env, exp))
        end
        | VAR id -> begin
            let rec envfind (id, env) =
                match env with
                | [] -> raise (Error "FreeVariable")
                | idval::env_rest ->
                    if fst idval = id then snd idval
                    else envfind (id, env_rest)
            in
            envfind (id, env)
        end
        | LET (id, exp1, exp2) -> eval((id, eval(env, exp1))::env, exp2)

    let print_value (v:value) = print_int v
end
