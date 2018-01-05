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

    let emptyEnv : environment = []
    let rec eval ((env:environment),(exp:expr)):value =
        let rec findInt str en=
            match en with
            | [] -> raise (Error "FreeVariable")
            | (i, num)::rest ->
                    if str = i then num
                    else findInt str rest
        in

        let rec evalMax max expList =
            match expList with
            | [] -> max
            | exp1::rest ->
                    let temp = (eval (env,exp1)) in
                    if (max<temp) then (evalMax temp rest)
                    else (evalMax max rest)
        in

        match exp with
        | NUM n -> n
        | PLUS (exp1, exp2) -> (eval (env,exp1)) + (eval (env, exp2))
        | MINUS (exp1, exp2) -> (eval (env,exp1)) - (eval (env, exp2))
        | MULT (exp1, exp2) -> (eval (env,exp1)) * (eval (env, exp2))
        | DIVIDE (exp1, exp2) -> (eval (env,exp1)) / (eval (env, exp2))
        | MAX [] -> raise (Error "NoExpressionsInMAX")
        | MAX (exp1::rest) -> (evalMax (eval (env,exp1)) rest)
        | VAR i -> (findInt i env)
        | LET (i,exp1,exp2) -> (eval (((i,(eval (env,exp1)))::env),exp2))

    let int_of_value (v:value) : int = v

end

