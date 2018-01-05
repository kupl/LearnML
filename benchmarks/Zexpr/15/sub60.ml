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
  type environment = (id * value) list

  let emptyEnv = []

  let rec idSearch : environment * id -> value = fun (env, id) ->
    match env with
    | [] -> raise (Error "FreeVariable")
    | (i, v)::t -> if i = id then v else idSearch (t, id)

  let rec eval : environment * expr -> value = fun (env, exp) ->
    match exp with
    | NUM i -> i
    | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
    | MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
    | MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
    | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
    | MAX [] -> 0
    | MAX (eh::[]) -> (eval (env, eh))
    | MAX (eh::et) -> 
      let x = (eval (env, eh))
      and y = (eval (env, MAX et)) in
      if x > y then x else y
    | VAR id -> idSearch (env, id)
    | LET (id, e1, e2) -> eval ((id, (eval (env, e1)))::env, e2)
  
  let print_value : value -> unit = fun v -> print_int v

end 

