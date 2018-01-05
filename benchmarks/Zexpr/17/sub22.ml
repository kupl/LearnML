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
    
    let my_max (l : value list) : int  = 
      match l with
      |[] -> 0
      |hd::tl -> List.fold_left max hd tl 

    let emptyEnv : environment = []
    let rec eval : environment * expr -> value = fun (env, ex) ->
      match ex with
      |NUM i -> i
      |PLUS (ex1, ex2) -> eval(env, ex1) + eval(env, ex2)
      |MINUS (ex1, ex2) -> eval(env, ex1) - eval(env, ex2)
      |MULT (ex1, ex2) -> eval(env, ex1) * eval(env, ex2)

      |DIVIDE (ex1, ex2) -> if eval(env, ex2) == 0 then raise (Error "error")
                            else eval(env, ex1) / eval(env, ex2) 

      |MAX list -> let eval_wrapper x = eval(env, x)
                   in my_max(List.map eval_wrapper list)

      |VAR x -> if List.mem_assoc x env then List.assoc x env
                else raise (Error "FreeVariable")

      |LET (id, ex1, ex2) -> eval((id, eval(env,ex1))::env, ex2)
  
    let print_value : value -> unit = fun a -> print_endline(string_of_int(a))
end 
