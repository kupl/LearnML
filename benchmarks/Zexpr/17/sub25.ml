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
    type value
    val emptyEnv: environment
    val eval: environment * expr -> value
    val print_value: value -> unit
  end

module Zexpr : ZEXPR =
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
    type environment = int list
    type value = int
    let emptyEnv = []
    let substitute (var, exp1, exp2) =
      let rec sub exp2 =
        match exp2 with
        NUM n -> NUM n
        |PLUS (expr1, expr2) -> PLUS (sub expr1, sub expr2)
        |MINUS (expr1, expr2) -> MINUS (sub expr1, sub expr2)
        |MULT (expr1, expr2) -> MULT (sub expr1, sub expr2)
        |DIVIDE (expr1, expr2) -> DIVIDE (sub expr1, sub expr2)
        |MAX l -> MAX (List.map sub l)
        |VAR id -> if var = id then exp1 else exp2
        |LET (id, expr1, expr2) -> LET (id, sub expr1, if var = id then expr2 else sub expr2)
      in sub exp2

    let rec max_list l =
      match l with
      [] -> 0
      |[h] -> h
      |h::t -> max h (max_list t)

    let rec eval_helper expr =
      match expr with  
      NUM n -> n
      |PLUS (exp1, exp2) -> (eval_helper exp1) + (eval_helper exp2)
      |MINUS (exp1, exp2) -> (eval_helper exp1) - (eval_helper exp2)
      |MULT (exp1, exp2) -> (eval_helper exp1) * (eval_helper exp2)
      |DIVIDE (exp1, exp2) -> (eval_helper exp1) / (eval_helper exp2)
      |MAX eList -> max_list (List.map eval_helper eList)
      |VAR id -> raise (Error "FreeVariable")
      |LET (id, exp1, exp2) -> eval_helper(substitute(id, exp1, exp2))

    let rec eval ((env: environment), (expr: expr)) : value  =
      eval_helper expr

    let print_value = print_int
  end

