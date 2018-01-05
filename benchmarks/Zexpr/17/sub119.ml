module Zexpr = struct
    exception Error of string
    type id = string
    type expr   = NUM of int
                | PLUS of expr * expr
                | MINUS of expr * expr
                | MULT of expr * expr
                | DIVIDE of expr * expr
                | MAX of expr list
                | VAR of id
                | LET of id * expr * expr
    type environment = (string * int) list 
    type value = int
    let emptyEnv = []
    let rec eval(env, e) =
        match e with
        | NUM x -> x
        | PLUS (x, y) -> eval(env, x) + eval(env, y)
        | MINUS(x, y) -> eval(env, x) - eval(env, y)
        | MULT (x, y) -> eval(env, x) * eval(env, y)
        | DIVIDE(x,y) -> eval(env, x) / eval(env, y)
        | MAX l -> begin
            let max (a, b) =
                if a > b then a
                else b
            in
            let rec maxlist ll =
                match ll with
                | [] -> 0
                | [elem] -> eval(env, elem)
                | elem :: rest -> max(eval(env, elem), (maxlist rest))
            in
            maxlist l
        end
        | VAR x -> begin
            let rec getvalue (l: environment)  =
                match l with
                | [] -> (raise(Error "fail: no such value"))
                | (ls, lv) :: rest -> if x = ls then lv
                                      else getvalue(rest)
            in
            getvalue(env)
        end
        | LET(name,value,ex) -> eval(((name, eval(env, value))::env), ex)

    end

