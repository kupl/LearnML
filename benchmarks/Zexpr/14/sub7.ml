module type ZEXPR = sig
  exception Error of string
  type id = string
  type expr =
    NUM of int
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

module Zexpr:ZEXPR = struct
  exception Error of string
  type id = string
  type expr =
    NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
    | MULT of expr * expr
    | DIVIDE of expr * expr
    | MAX of expr list
    | VAR of id
    | LET of id * expr * expr

  type value = VALUE of float
  type environment = env_ele list
    and env_ele = ENV of id * value

  let emptyEnv = []
  let int_of_value v = match v with VALUE f -> int_of_float f
  let value_of_int i = VALUE (float_of_int i)
  let rec eval(env, exp) = match exp with
    NUM i -> (value_of_int i)
    | PLUS (e1, e2) -> value_of_int ((int_of_value (eval (env, e1))) + (int_of_value (eval (env, e2))))
    | MINUS (e1, e2) -> value_of_int ((int_of_value (eval (env, e1))) - (int_of_value (eval (env, e2))))
    | MULT (e1, e2) -> value_of_int ((int_of_value (eval (env, e1))) * (int_of_value (eval (env, e2))))
    | DIVIDE (e1, e2) -> value_of_int ((int_of_value (eval (env, e1))) / (int_of_value (eval (env, e2))))
    | VAR x ->
      let rec find_value_of_var (env, v) =
        match env with
        [] -> raise (Error "FreeVariable")
        | (ENV (var, value_of_var))::elist ->
          if String.compare v var == 0
            then value_of_var
          else
            find_value_of_var (elist, v) in
      find_value_of_var (env, x)
    | LET (var, expr_of_var, e) -> 
      eval(ENV (var, eval(env, expr_of_var))::env, e)
    | MAX (elist) ->
      match elist with
      [] -> value_of_int 0
      |_ ->
        let rec find_max_expr (elist, now_max) = 
          match elist with
          [] -> now_max
          | e::elist2 ->
            let now_value = eval(env, e) in
            let value_compare (v1, v2) = 
              match (v1, v2) with (VALUE f1, VALUE f2) -> (int_of_float (f1 -. f2)) in
            if value_compare (now_value, now_max) > 0
              then find_max_expr (elist2, now_value)
            else
              find_max_expr (elist2, now_max) in
        find_max_expr (elist, eval(env, List.hd (elist)))
end
