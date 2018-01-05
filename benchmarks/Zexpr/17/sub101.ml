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
  
  module StringMap = Map.Make(String)
  type value = int
  type environment = value StringMap.t

  let emptyEnv = StringMap.empty
  let rec eval (env, e) : value =
    match e with
    | NUM i -> i
    | PLUS   (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
    | MINUS  (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
    | MULT   (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
    | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
    | MAX l -> if l = [] then 0
      else List.fold_left (fun m e' -> max m (eval (env, e'))) min_int l
    | LET (id, se, e') -> eval (StringMap.add id (eval (env, se)) env, e')
    | VAR id ->
      try StringMap.find id env
      with Not_found -> raise (Error "FreeVariable")
    
  let print_value v = print_endline (string_of_int v)
end 


