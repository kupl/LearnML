module type ZEXPR =
  sig
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

    val emptyEnv: environment
    val eval: environment * expr -> value

    val int_of_value: value -> int
  end 

module Zexpr: ZEXPR =
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

    let emptyEnv = []
    let rec eval (env, e) = 
            match e with
            | NUM(i) -> i
            | PLUS(e1, e2) -> eval(env, e1) + eval(env, e2)
            | MINUS(e1, e2) -> eval(env, e1) - eval(env, e2)
            | MULT(e1, e2) -> eval(env, e1) * eval(env, e2)
            | DIVIDE(e1, e2) -> eval(env, e1) / eval(env, e2)
            | MAX(elist) -> if List.length elist = 0
                             then 0
                             else List.fold_left (fun x e1 -> if x >= eval(env,e1)
            then x else eval(env, e1)) (eval(env,(List.hd elist))) (List.tl elist)
            | VAR(s) -> if List.mem_assoc s env
                         then List.assoc s env
                         else raise (Error "FreeVariable")
            | LET(s, e1, e2) -> eval((s,eval(env, e1))::(List.remove_assoc s env), e2)            

    let int_of_value v = v
  end   
