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

    module IdMap_ = Map.Make(String)
    type value = int
    type environment = value IdMap_.t

    let emptyEnv = IdMap_.empty
    let rec eval: (environment * expr) -> value = fun (env, x) ->
      match x with
        | NUM(i) -> i
        | PLUS(e1, e2) -> (eval (env, e1)) + (eval (env, e2))
        | MINUS(e1, e2) -> (eval (env, e1)) - (eval (env, e2))
        | MULT(e1, e2) -> (eval (env, e1)) * (eval (env, e2))
        | DIVIDE(e1, e2) -> (eval (env, e1)) / (eval (env, e2))
        | MAX(el) -> (
            let max_list: value list -> value = fun l ->
              match l with
                | [] -> 0
                | hd::tl -> (List.fold_left Pervasives.max hd tl)
            in
            max_list (List.map (fun e -> eval (env, e)) el)
          )
        | VAR(id) -> (
            try (IdMap_.find id env)
            with Not_found -> raise (Error "FreeVariable")
          )
        | LET(id, e1, e2) -> (
            let e1val = eval (env, e1) in
            eval (IdMap_.add id e1val env, e2)
          )

    let print_value = fun v ->
      print_endline (Format.sprintf "%d" v)
  end
