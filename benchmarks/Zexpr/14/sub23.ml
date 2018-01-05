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
    val int_of_value: value -> int
  end

module Zexpr: ZEXPR =
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

    type value = VALUE of int
    type environment = ENV of (id * value) list
    let emptyEnv = ENV []
    let rec eval env_expr =
      let rec _eval = function
        | (env, NUM int) ->
           NUM int
        | (env, PLUS (NUM int_a, NUM int_b)) ->
           NUM (int_a + int_b)
        | (env, PLUS (expr_a, expr_b)) ->
           let eval_a = _eval (env, expr_a) in
           let eval_b = _eval (env, expr_b) in
           _eval (env, PLUS (eval_a, eval_b))

        | (env, MINUS (NUM int_a, NUM int_b)) ->
           NUM (int_a - int_b)
        | (env, MINUS (expr_a, expr_b)) ->
           let eval_a = _eval (env, expr_a) in
           let eval_b = _eval (env, expr_b) in
           _eval (env, MINUS (eval_a, eval_b))

        | (env, MULT (NUM int_a, NUM int_b)) ->
           NUM (int_a * int_b)
        | (env, MULT (expr_a, expr_b)) ->
           let eval_a = _eval (env, expr_a) in
           let eval_b = _eval (env, expr_b) in
           _eval (env, MULT (eval_a, eval_b))

        | (env, DIVIDE (NUM int_a, NUM int_b)) ->
           NUM (int_a / int_b)
        | (env, DIVIDE (expr_a, expr_b)) ->
           let eval_a = _eval (env, expr_a) in
           let eval_b = _eval (env, expr_b) in
           _eval (env, DIVIDE (eval_a, eval_b))

        | (env, MAX []) ->
           NUM 0
        | (env, MAX [expr]) ->
           _eval (env, expr)
        | (env, MAX (head::value_list)) ->
           let eval_head = _eval (env, head) in
           let max_else = _eval (env, MAX value_list) in
           if eval_head < max_else then
             max_else
           else
             eval_head

        | (ENV [], VAR id) ->
           raise (Error "FreeVariable")
        | (ENV (head::env_list), VAR id) ->
           (match head with
            | (env_id, VALUE int) when env_id = id ->
               NUM int
            | _ ->
               _eval (ENV env_list, VAR id)
           )

        | (ENV env_list, LET (id, expr_a, expr_b)) ->
           let eval_a = eval (ENV env_list, expr_a) in
           let new_env_list = (id, eval_a) :: env_list in
           _eval (ENV new_env_list, expr_b)
      in

      let calc_expr = _eval env_expr in

      match calc_expr with
      | NUM int -> VALUE int
      | _ -> raise (Error "FreeVariable")
    ;;
    let int_of_value = function
      | VALUE int -> int
    ;;
  end
