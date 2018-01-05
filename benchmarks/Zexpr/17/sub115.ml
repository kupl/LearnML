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

        type environment = (id * int) list
        type value = int

        let emptyEnv = []
        let rec eval (env, expr) =
            match expr with
            | NUM n -> n
            | PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
            | MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
            | MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
            | DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
            | MAX expr_list ->
                let rec get_max: value * expr list -> value = fun (x, e_list) ->
                    match e_list with
                    | [] -> x
                    | hd::tl ->
                        let y = eval (env, hd) in
                        if x >= y then get_max (x, tl) else get_max (y, tl) in
                (match expr_list with
                | [] -> 0
                | hd::tl -> get_max (eval (env, hd), tl)
                )
            | VAR id -> (try List.assoc id env with Not_found -> raise (Error "FreeVariable"))
            | LET (id, e1, e2) -> eval ((List.append [id, (eval (env, e1))] env), e2)

        let print_value (value) = print_endline (string_of_int value)
    end
