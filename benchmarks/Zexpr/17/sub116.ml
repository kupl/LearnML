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
        val emptyEnv: environment
        val eval: environment * expr -> value
    end

module Zexpr: ZEXPR =
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
        type environment = (string * expr) list
        type value = int
        let emptyEnv = []

        let rec eval ((env: environment), (e: expr)) : value =
            match e with
            | NUM(n) -> n
            | PLUS(e1, e2) -> (eval (env, e1)) + (eval (env, e2))
            | MINUS(e1, e2) -> (eval (env, e1)) - (eval (env, e2))
            | MULT(e1, e2) -> (eval (env, e1)) * (eval (env, e2))
            | DIVIDE(e1, e2) -> (eval (env, e1)) / (eval (env, e2))
            | MAX(el) ->
                    let rec maxEval (el: (expr list)) (m: value) : value =
                        match el with
                        | [] -> m
                        | eh::et -> let eveh = (eval (env, eh)) in
                        if eveh > m then maxEval et eveh else maxEval et eveh in
                    (match el with
                    | [] -> 0
                    | eh::et -> maxEval et (eval (env, eh)))
            | VAR(i) ->
                    let rec getVar (d: id) (environ: environment) =
                        match environ with
                        | [] -> raise (Error "no variable")
                        | (s, e)::t -> if d = s then eval (environ, e) else getVar d t in
                    getVar i env
            | LET(d, e1, e2) -> eval (((d, e1)::env), e2)
    end
