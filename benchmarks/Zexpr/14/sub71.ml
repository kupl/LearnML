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
  val emptyEnv : environment
  val eval : environment * expr -> value
  val int_of_value : value -> int
 end

module Zexpr = 
 struct
  exception Error of string
  exception FreeVariable
  type id = string
  type value = int
  type expr = 
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MULT of expr * expr
  | DIVIDE of expr * expr
  | MAX of expr list
  | VAR of id
  | LET of id * expr * expr
  type environment = (id * value) list 
  let emptyEnv = []
  let rec max list ret order = match list with
  | [] -> if(order=0) then 0 else ret
  | NUM(n)::l -> if(order=0) 
  then (max l n 1) 
  else if (n>ret) 
	  then (max l n 1) 
	  else (max l ret 1)
  let int_of_value n = n
  let rec eval (env,expr) = match expr with
  | NUM(n) -> n
  | PLUS(a,b) -> eval(env,a) + eval(env,b)
  | MINUS(a,b) -> eval(env,a) - eval(env,b)
  | MULT(a,b) -> eval(env,a) * eval(env,b)
  | DIVIDE(a,b) -> eval(env,a) / eval(env,b)
  | VAR(id) -> begin
  try(
  snd (List.find (fun (i, v) -> if(i=id) then true else false ) env)
  ) with (Not_found) -> raise (FreeVariable)
  end
  | LET(id,va,expr) -> eval((id,eval(env,va))::env,expr)
  | MAX(list) -> max list 0 0
 end
