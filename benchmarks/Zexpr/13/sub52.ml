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
end

module Zexpr : ZEXPR = struct
  exception Error of string
  exception FreeVariable
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
  let eval (env, e) = 
    match e with
    |NUM x -> x
    |PLUS (l,r) -> (eval (env,l))+(eval (env,r))
    |MINUS (l,r) -> (eval (env,l))-(eval (env,r))
    |MULT (l,r) -> (eval (env,l))*(eval (env r))
    |DIVIDE (l,r) -> (eval (env,l)/(eval (env r))
    |MAX list -> if list=[] then 0
    else if (List.tl list)=[] then eval(env,List.hd list)
    else if (eval(env,List.hd list)>eval(env,MAX (List.tl list)))
    then eval(env,List.hd list)
	else eval(env,MAX (List.tl list))
    |VAR str -> if env=emptyEnv then raise FreeVariable
    else if (fst (List.hd env))=str then snd (List.hd env) else eval (List.tl env,e)
    |LET(str,x,y) -> eval((str,eval (env,x))::env,y)
  let int_of_value v = v
end
