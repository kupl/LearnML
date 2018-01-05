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
    type environment = (id * int) list
    type value = int
    let emptyEnv: environment = []
    let rec eval (env, expr) = 
      match expr with
      | NUM a -> a
      | PLUS (b, c) -> eval (env, b) + eval (env, c)
      | MINUS (d, e) -> eval (env, d) - eval (env, e)
      | MULT (f, g) -> eval (env, f) * eval (env, g)
      | DIVIDE (h, i) -> eval (env, h) / eval (env, i)
      | MAX li -> ( 
          let rec get_max: (expr list * int) -> int = fun (l, m) ->
            match l with
            | [] -> m
            | h::t -> if (eval (env, h) > m) then (get_max (t, eval (env, h)))
                      else (get_max (t, m)) in
          match li with
          | [] -> 0
          | h::t -> get_max (t, eval (env, h))
      )
      | VAR l -> if (List.exists (fun (a, b) -> a = l) env) 
                 then snd (List.find (fun (a, b) -> a = l) env)
                 else raise (Error "FreeVariable")
      | LET (m, n, o) -> eval ((m, eval(env, n))::env, o) 
    let print_value v = print_int v
  end

