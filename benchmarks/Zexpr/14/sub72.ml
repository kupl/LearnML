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
  val eval : environment * expr -> value 

  val int_of_value : value -> int 
end;;

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

  type value = VAL of int
  type environment = id * expr list
  
  let emptyEnv = []
  let int_of_value (VAL v) = v

  let rec eval (b,e) = match (b,e) with
    | (env, NUM x) -> VAL x
    | (env, PLUS (e1,e2)) -> VAL (int_of_value (eval (env,e1)) + int_of_value (eval (env,e2)))
    | (env, MINUS (e1,e2)) -> VAL (int_of_value (eval (env,e1)) - int_of_value (eval (env,e2)))
    | (env, MULT (e1,e2)) -> VAL (int_of_value (eval (env,e1)) * int_of_value (eval (env,e2)))
    | (env, DIVIDE (e1,e2)) -> VAL (int_of_value (eval (env,e1)) / int_of_value (eval (env,e2)))
    | (env, MAX []) -> VAL 0
    | (env, MAX (t::lst)) -> find_max(env,eval(env,t),lst)
    | (env, VAR s) -> eval_var(env,s)
    | (env, LET (s, e1, e2)) -> eval ( (s,e1)::env, e2)
  and find_max (b,u,l) = match (b,u,l) with
    | (env,mmax,[]) -> mmax
    | (env,mmax,t::lst) -> 
        let tmp = eval(env,t) in
        if int_of_value mmax<int_of_value tmp then find_max (env,tmp,lst) else find_max (env,mmax,lst)
  and eval_var (b,s) = match (b,s) with
    | ([],var) -> raise (Error "FreeVariable")
    | (t::env,var) -> if fst t = var then eval (env, snd t) else eval_var(env,var)
end;;