(* SPEC: MAX [] -> 0 *)

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

module Zexpr =
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

    let rec eval: environment * expr -> value =
      fun (env, exp) ->
        let rec max_aux: expr list * value -> value =
          fun (el_a, v_a) ->
            match el_a with
            | [] -> v_a
            | hd::tl -> max_aux (tl, max (eval (env, hd)) v_a) in
        let rec var_aux: environment * id -> value =
          fun (e, i) ->
            match e with
            | [] -> raise (Error "FreeVariable")
            | hd::tl ->
                (match hd with
                 | (hd_i, hd_v) when hd_i = i -> hd_v
                 | _ -> var_aux (tl, i)) in
        match exp with
        | NUM x -> x
        | PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
        | MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
        | MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
        | DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
        | MAX [] -> 0
        | MAX el -> max_aux (el, min_int)
        | VAR i -> var_aux (env, i)
        | LET (i, e_i, e) -> eval ((i, eval (env, e_i))::env, e)

    let int_of_value: value -> int =
      fun v -> v
  end
