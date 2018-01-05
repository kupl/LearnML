module type ZEXPR = 
    sig
        exception Error of string
        type id = string
        type expr = 
            |NUM of int
            |PLUS of expr * expr
            |MINUS of expr * expr
            |MULT of expr * expr
            |DIVIDE of expr * expr
            |MAX of expr list
            |VAR of id
            |LET of id * expr * expr
        type environment
        type value
        val emptyEnv: environment
        val eval: environment * expr -> value
    end

module Zexpr : ZEXPR =
    struct
        exception Error of string
        type id = string
        type expr = 
            |NUM of int
            |PLUS of expr * expr
            |MINUS of expr * expr
            |MULT of expr * expr
            |DIVIDE of expr * expr
            |MAX of expr list
            |VAR of id
            |LET of id * expr * expr
        type value = int
        type environment = (id * value) list
        let emptyEnv = []
        let rec findVar (env, i) =
            match env with
            |[] -> raise (Error "Variable not found")
            |h::t ->(
                    match h with
                    |(x, y) ->  if x = i then y
                                else findVar(t, i)
            )
        let rec maxList (env, p, l) =
            match l with
            |[] -> p
            |h::t ->    if eval(env, h) > p then maxList(env, eval(env, h), t)
                        else maxList(env, p, t)
        and eval (env, exp) =
            match exp with
            |NUM n -> n
            |PLUS (a, b)-> eval(env, a) + eval(env, b)
            |MINUS (a, b)-> eval(env, a) - eval(env, b)
            |MULT (a, b)-> eval(env, a) * eval(env, b)
            |DIVIDE (a, b)-> eval(env, a) / eval(env, b)
            |MAX l -> (
                match l with
                |[] -> 0
                |h::t -> maxList(env, eval(env, h), t)
                ) 
            |VAR i ->  findVar(env, i) 
            |LET (i, exp1, exp2)->  let newEnv = (i, eval(env, exp1))::env in
                                    eval (newEnv, exp2)
    end
