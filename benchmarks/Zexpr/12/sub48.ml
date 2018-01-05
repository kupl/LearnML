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

    type environment = (id * int) list
    type value = int

    val emptyEnv: environment
    val eval: environment * expr -> value

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
    type environment = (id * int) list

    let emptyEnv = []

        let rec eval2 (env, e) =

                let rec find_value (env, v) =
                match env with
                | [] -> raise (Error "no var")
                | (a, b)::tl -> if a = v then b else find_value (tl, v)

                in
                let rec maxi (env, lst) =
                        match lst with
                        |[] -> 0
                        |hd::tl -> if (eval2 (env, hd)) > (maxi (env,tl)) then (eval2 (env, hd)) else (maxi (env, tl))
                in

                match e with
                | NUM a -> a
                | PLUS (a, b) -> (eval2 (env,a)) + (eval2 (env, b))
                | MINUS (a, b) -> (eval2 (env,a)) - (eval2 (env, b))
                | MULT (a, b) -> (eval2 (env,a)) * (eval2 (env, b))
                | DIVIDE (a, b) -> (eval2 (env, a)) / (eval2 (env,b))
                | MAX a -> (maxi (env,a))
                | VAR a -> (find_value (env, a))
                | LET (a, b, c) -> eval2 ((((a, (eval2 (env, b)))::env), c))


        let eval (env, e) = print_int (eval2 (env, e)) ; (eval2 (env,e))

end

