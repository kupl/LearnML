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


module Zexpr (*: ZEXPR *) = 
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

  let varlist : (id * expr) list = []

  let emptyEnv : environment = []

  let rec eval ((env : environment),(ex : expr)) : value =
    match ex with
    | NUM num -> num
    | PLUS (a, b) -> (eval (env,a))+(eval (env,b))
    | MINUS (a, b) -> (eval (env,a))-(eval (env,b))
    | MULT (a, b) -> (eval (env,a))*(eval (env,b))
    | DIVIDE (a, b) -> (eval (env,a))/(eval (env,b))
    | MAX exl ->
    (
      match exl with
      | [] -> 0
      | exlhd::exltl ->
      (
        if exltl = [] then eval(env, exlhd)
        else if eval(env, exlhd) > eval(env, (MAX exltl)) then eval(env, exlhd)
        else eval(env, (MAX exltl))
      )
    )
    | VAR var ->
    (
      if List.mem_assoc var env then List.assoc var env
      else raise (Error "FreeVariable")
    )
    (*| VAR var ->
      try List.assoc var env with Not_found -> raise (Error "FreeVariable")*)
    | LET (var , (ex1 : expr), (ex2 : expr)) -> eval((var, eval(env, ex1))::env, ex2)

    let print_value : value -> unit = fun dout -> 
    print_int dout

end

