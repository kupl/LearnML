(*
  Department : Electrical Engineering
  Student-Id : 2008-11923
  Name : HyeonIL Choi (최현일)
  Date: 2017-9-23
  Homework-# : 2-7
  Excercise-Name : Zexpr
*)

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

  val emptyEnv : environment 
  val eval : environment * expr -> value 

  val print_value : value -> unit 
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

  type value = int
  type environment = (id*value) list
  
  let emptyEnv : environment = []
  let rec eval (env,expr) : value = (
    match expr with
    | NUM i -> i
    | PLUS (expr1, expr2) -> eval(env, expr1) + eval(env, expr2)
    | MINUS (expr1, expr2) -> eval(env, expr1) - eval(env, expr2)
    | MULT (expr1, expr2) -> eval(env, expr1) * eval(env, expr2)
    | DIVIDE (expr1, expr2) -> eval(env, expr1) / eval(env, expr2)
    | MAX exprlist -> (
      match exprlist with 
      | [] -> 0
      | fst::tl -> (
        let fstValue = eval(env, fst) in
        let rec evalMax exprl (maximum:value) = (
          match exprl with
          | [] -> maximum
          | hd::tl -> (
            let num = eval (env, hd) in
            if num > maximum 
            then evalMax tl num 
            else evalMax tl maximum
          )
        ) in 
        evalMax tl fstValue
      )
    )
    | VAR id -> (
      match env with 
      | [] -> raise (Error "FreeVariable")
      | (hdId, hdVal)::tl -> (
        if hdId = id 
        then hdVal
        else eval (tl, expr)
      )
    ) 
    | LET (id, defExpr, usageExpr) -> (
      let defVal = eval (env, defExpr) in  
      eval ((id,defVal)::env, usageExpr)
    )
  )

  let print_value value = print_int value; print_newline();


     

end 
