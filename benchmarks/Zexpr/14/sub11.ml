module type ZEXPR = sig
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

    val emptyEnv : environment
    val eval : environment * expr -> value (* 고정 *)

    val int_of_value : value -> int (* 고정 *)
end

module Zexpr = struct
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
    type environment = (string * int) list (* 넣기 *)
    type value = int (* 확정 *)
  
    let emptyEnv: environment = [] (* 넣기 *)

    let rec max_list l = (* 완벽 *)
      match l with
        | [] -> 0
        | h :: t -> max h (max_list t)

    let rec global_value_add id value global =
      let emptylist = [] in 
        match global with
          | ((myid,_) :: t) when myid = id -> (myid, value) :: t
          | (h :: t)                       -> h :: (global_value_add id value t)
          | emptylist                      -> (id, value) :: emptylist


    let rec eval arg =
      let global = fst(arg) in
        let expr = snd(arg) in
          match expr with
            | NUM a                -> a
            | PLUS (exp1, exp2)    -> eval (global, exp1) + eval (global, exp2)
            | MINUS (exp1, exp2)   -> eval (global, exp1) - eval (global, exp2)
            | MULT (exp1, exp2)    -> eval (global, exp1) * eval (global, exp2)
            | DIVIDE (exp1, exp2)  -> eval (global, exp1) / eval (global, exp2)
            | MAX expr_list        -> let values = List.map (fun a -> (eval (global, a))) (expr_list) in
                                        let f = (max_list values) in
                                          f
            | VAR id               -> let mycheck =
                                        try 
                                          List.assoc id global (* 확정 *)
                                        with Not_found -> raise (Error "FreeVariable") in
                                          mycheck
            | LET (id, exp1, exp2) -> let value = eval (global, exp1) in
                                        let f = (global_value_add id value global) in
                                          eval (f, exp2)

    let int_of_value v = v
end



