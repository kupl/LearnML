module type ZEXPR =
    sig
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

let list_max lst = match lst with
    | hd::tl -> List.fold_left max hd tl
    | [] -> invalid_arg "Tried to get maximum element of empty list!"

module Zexpr : ZEXPR =
    struct
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

        type value = int
        type environment = (id * value) list
        let emptyEnv = []

        let add_env env key value = (key, value)::env
        let find_env env key = 
            try let (_, value) = List.find (fun (k, v) -> k = key) env in value
            with Not_found -> raise (Error "FreeVariable")

        let rec eval (env, expr) = match expr with
            | NUM(value) -> value
            | PLUS(expr1, expr2) -> (eval (env, expr1)) + (eval (env, expr2))
            | MINUS(expr1, expr2) -> (eval (env, expr1)) - (eval (env, expr2))
            | MULT(expr1, expr2) -> (eval (env, expr1)) * (eval (env, expr2))
            | DIVIDE(expr1, expr2) -> (eval (env, expr1)) / (eval (env, expr2))
            | MAX([]) -> 0
            | MAX(expr_list) -> (
                let calculated_exprs = List.map (fun e -> eval (env, e)) expr_list in
                list_max calculated_exprs
            )
            | VAR(id) -> find_env env id
            | LET(id, expr1, expr2) -> (
                let value = eval (env, expr1) in
                let env' = add_env env id value in
                eval (env', expr2)
            )

        let print_value n = print_string (string_of_int n)
    end
