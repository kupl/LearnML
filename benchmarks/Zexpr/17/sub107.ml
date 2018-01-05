(*
    Homework 2, Exercise 7
    2015-15894 Jonghoon Won
    Sep 28, 2017
*)

module type ZEXPR = sig
    exception Error of string
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
    val print_value: value -> unit
end

module Zexpr: ZEXPR = struct
    exception Error of string
    type id = string
    type expr = NUM of int
              | PLUS of expr * expr
              | MINUS of expr * expr
              | MULT of expr * expr
              | DIVIDE of expr * expr
              | MAX of expr list
              | VAR of id
              | LET of id * expr * expr
    type environment = (id * int) list
    type value = int

    let emptyEnv: environment = []
    let rec eval: environment * expr -> value =  fun (e, exp) ->
        match exp with
              NUM(n) -> n
            | PLUS(exp1, exp2) -> (eval (e, exp1)) + (eval (e, exp2))
            | MINUS(exp1, exp2) -> (eval (e, exp1)) - (eval (e, exp2))
            | MULT(exp1, exp2) -> (eval (e, exp1)) * (eval (e, exp2))
            | DIVIDE(exp1, exp2) -> (eval (e, exp1)) / (eval (e, exp2))
            | MAX(exps) -> (
                let rec findMaxHelper: expr list * value -> value = fun (exps, current_max) ->
                    match exps with
                          [] -> current_max
                        | exp::tl -> (
                            let v = eval (e, exp) in
                            if v > current_max then findMaxHelper (tl, v)
                            else findMaxHelper (tl, current_max)
                            )
                in
                match exps with
                      [] -> 0
                    | hd::tl -> findMaxHelper (tl, eval (e, hd))
                )
            | VAR(name) -> (
                let rec searchEnvironment: environment * id -> value = fun (e, name) ->
                    match e with
                      [] -> raise (Error "FreeVariable")
                    | hd::tl -> (
                        if (fst hd) = name then (snd hd)
                        else searchEnvironment (tl, name)
                        )
                in searchEnvironment (e, name)
                )
            | LET(name, exp1, exp2) -> (
                let rec append: environment * environment * environment -> environment = fun (e1, e2, result) ->
                    match (e1, e2) with
                          ([], []) -> result
                        | (hd::tl, _) -> append (tl, e2, hd::result)
                        | ([], hd::tl) -> append ([], tl, hd::result)
                in

                let rec updateEnvironment: environment * environment * (id * int) -> environment =
                    fun (e, result_e, (name, v)) ->
                        match e with
                              [] -> (name, v)::result_e
                            | hd::tl -> (
                                if (fst hd) = name then append ((name, v)::tl, result_e, [])
                                else updateEnvironment (tl, hd::result_e, (name, v))
                                )
                in
                let updatedEnvironment = updateEnvironment (e, [], (name, eval (e, exp1))) in
                eval (updatedEnvironment, exp2)
                )

    let print_value: value -> unit = fun v ->
        Printf.printf "%d\n" v
end
