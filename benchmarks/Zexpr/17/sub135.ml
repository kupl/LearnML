exception Error of string

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
  type environment = id -> value

      
  let emptyEnv : environment = fun x -> raise (Error "FreeVariable")  

  let rec eval : environment * expr -> value = function (env, exp) ->
  match exp with
  | NUM i -> i
  | PLUS (exp1, exp2) -> eval (env, exp1) + eval (env, exp2)
  | MINUS (exp1, exp2) -> eval (env, exp1) - eval (env, exp2)
  | MULT (exp1, exp2) -> eval (env, exp1) * eval (env, exp2)
  | DIVIDE (exp1, exp2) ->
    let v1= eval (env, exp1) in
    let v2 = eval (env, exp2) in
    if v2 = 0 then raise (Error "Division by Zero")
    else v1/v2
  | MAX lst ->
    let l' = List.map (fun x -> eval (env, x)) lst in
    List.fold_left (fun x y -> if (y>x) then y else x) (List.hd l') l'
  | VAR id -> env id
  | LET (id, exp1, exp2) ->
    let n_env = fun x -> if x = id then eval (env, exp1) else env x in
    eval (n_env, exp2)

let print_value : value -> unit = function x ->
    print_int x
  
end 
