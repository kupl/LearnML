(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 2, Exercise 7 *)

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
        type environment = (id, value) Hashtbl.t

        let emptyEnv = Hashtbl.create 0
        let print_value value = print_endline (string_of_int value)
        let rec eval (env, exp) =
            let rec max comparator lst =
                match lst with
                | [] -> NUM 0
                | e::rest ->
                        match rest with
                        | [] -> e
                        | _ ->
                                let maxOfRest = max comparator rest in
                                if comparator e maxOfRest then e
                                else maxOfRest
            in
            match exp with
            | NUM value -> value
            | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
            | MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
            | MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
            | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
            | MAX elist -> eval (env, max (fun e1 e2 -> (eval (env, e1) > (eval (env, e2)))) elist)
            | VAR id -> (try Hashtbl.find env id with
                    | Not_found -> raise (Error "FreeVariable"))
            | LET (id, value, expr) ->
                    let newEnv = Hashtbl.copy env in
                    let _ = (Hashtbl.add newEnv id (eval (env, value))) in
                    eval (newEnv, expr)
    end
