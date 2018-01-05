(* HW2 Exercise 7 Calculation *)

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

    val print_value : value -> unit
end

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
    type environment = (id * int) list
    type value = int
    let emptyEnv: environment = []
    let rec eval: environment * expr -> value = fun (env, e) ->
      match e with
      | NUM n -> n
      | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
      | MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
      | MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
      | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
      | MAX expr_list ->
        (match expr_list with
        | [] -> 0
        | head::tail -> (
            let first_value = eval(env, head) in
            let rec max_in_list: expr list -> value = fun expr_list ->
              match expr_list with
              | [] -> first_value
              | head::tail -> (
                  let head_value = eval(env, head) in
                  let tail_value = max_in_list(tail) in
                  if (head_value > tail_value) then head_value
                  else tail_value
                )
            in

            max_in_list expr_list
          )
        )
      | VAR id_checking -> (
          let rec find_id_in_env: environment * id -> value = fun (env, id_checking) ->
            match env with
            | [] -> raise (Error "FreeVariable")
            | head::tail -> (
                if (id_checking = fst head) then (snd head)
                else find_id_in_env (tail, id_checking)
              )
          in

          find_id_in_env (env, id_checking)
        )
      | LET (id_defining, e1, e2) -> (
          eval((id_defining, (eval (env, e1)))::env, e2)
        )
    let print_value : value -> unit = fun (v) ->
      Printf.printf "%d\n" v
end
