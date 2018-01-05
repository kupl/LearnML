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
    val emptyEnv : environment
    val eval : environment * expr -> value
    val print_value : value -> unit
  end

module Zexpr : ZEXPR =
  struct
    let rec print_env env =
      match env with
      | (id, value) ::t -> print_string id; print_string(", ");
                           print_int value; print_string("; ");
                           print_env t
      | [] -> print_newline()
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
    let emptyEnv : environment = []
    let rec eval (env, exp) =
      (match exp with
      | NUM i -> i
      | PLUS (e1, e2) -> eval(env, e1) + eval(env, e2)
      | MINUS (e1, e2) -> eval(env, e1) - eval(env, e2)
      | MULT (e1, e2) -> eval(env, e1) * eval(env, e2)
      | DIVIDE (e1, e2) -> eval(env, e1) / eval(env, e2)
      | MAX el ->
          (match el with
          | hd :: [] -> eval(env, hd)
          | hd :: tl ->
              if (eval(env, hd) > eval(env, MAX tl)) then eval(env, hd) else
                eval(env, MAX tl)
          | [] -> 0)
      | VAR id ->
          (let rec check_env id env =
            match env with
            | (env_id, env_value) :: t ->
              if (env_id = id) then env_value else (check_env id t)
            | [] -> raise (Error "FreeVariable")
          in
          check_env id env)
      | LET (id, e1, e2) ->
          (let n_env = (id, eval(env, e1)) :: env in
          eval(n_env, e2)))

    let print_value x = print_int x; print_newline()
  end


