module type ZEXPR = sig exception Error of string
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

  val emptyEnv: environment
  (* Execute expr and print result *)
  val eval: environment * expr -> value
  val int_of_value: value -> int
end

module Zexpr : ZEXPR =
  struct
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
    type environment = (string * int) list
    type value = int 

    let emptyEnv: environment = []
      
    (* Execute expr and print result *)
    let rec eval ((env: environment), (exp: expr)): value =
      match exp with
      | NUM n -> n
      | PLUS (exp1, exp2) -> eval (env, exp1) + eval (env, exp2)
      | MINUS (exp1, exp2) -> eval (env, exp1) - eval (env, exp2)
      | MULT (exp1, exp2) -> eval (env, exp1) * eval (env, exp2)
      | DIVIDE (exp1, exp2) -> eval (env, exp1) / eval (env, exp2)
      | MAX exp_list -> let compare (a: int) (b: int): int =
                          if a > b then -1
                          else if a < b then 1
                          else 0
                        in
                        let sorted_list = 
                          List.sort compare (List.map (function x -> eval (env, x)) exp_list)
                        in
                        if List.length sorted_list == 0
                          then 0
                          else List.hd sorted_list
      | VAR v -> (* env에 v가 있으면 변환 없으면 Error *)
                 if List.mem_assoc v env
                   then List.assoc v env
                   else raise (Error v)
      | LET (i, exp1, exp2) -> let val_i = eval (env, exp1) in
                               let env_ = List.remove_assoc i env in
                               eval ((i, val_i)::env_, exp2)

    let int_of_value (v: value): int = v
end
