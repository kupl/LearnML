module type ZEXPR =
  sig
    exception Error of string
    type id = string
    type expr = NUM of int
        |PLUS of expr * expr
        |MINUS of expr * expr
        |MULT of expr * expr
        |DIVIDE of expr * expr
        |MAX of expr list
        |VAR of id
        |LET of id * expr * expr
    type environment
    type value = int
    val emptyEnv: environment
    val eval: environment * expr -> value
    val print_value: value -> unit
  end

module Zexpr: ZEXPR =
    struct
        exception Error of string
        type id = string
        type expr = NUM of int
            |PLUS of expr * expr
            |MINUS of expr * expr
            |MULT of expr * expr
            |DIVIDE of expr * expr
            |MAX of expr list
            |VAR of id
            |LET of id * expr * expr
        type value = int
        type environment = (id * value) list
        let emptyEnv: environment = []
        let print_value x =
          print_int x
        let rec eval (env, expr) =
            let rec check id list =
              match list with
              |[] -> raise (Error "FreeVariable")
              |a::sublist -> if (fst a) = id then snd a
              else (check id sublist) in
        let max env list =
          if (List.length list) = 0 then (NUM 0) else
          let rec aux env list ret =
            match list with
            |[] -> ret
            |a::sublist -> if (eval (env, a)) > (eval (env, ret)) then (aux env sublist a) else (aux env sublist ret)
          in
          aux env list (List.hd list) in
            match expr with
            |NUM a -> a
            |PLUS (ex1, ex2) -> eval (env, ex1) + eval (env, ex2)
            |MINUS (ex1, ex2) -> eval (env, ex1) - eval (env, ex2)
            |MULT (ex1, ex2) -> eval (env, ex1) * eval (env, ex2)
            |DIVIDE (ex1, ex2) -> eval (env, ex1) / eval (env, ex2)
            |VAR id -> (check id env)
            |LET (id, ex1, ex2) -> List.remove_assq id env; eval ((id, (eval (env, ex1)))::env, ex2)
            |MAX list -> eval (env, (max env list))
    end
