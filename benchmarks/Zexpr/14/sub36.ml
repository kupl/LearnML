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

module Zexpr = 
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
      type environment = (id * expr) list
      type value = int
      let emptyEnv = []
      
      let rec getExpr(env, id) =
               if env = [] then raise (Error "FreeVariable")
               else let e::elist = env in let (i,ex) = e in
                        if i = id then ex
                        else getExpr(elist,id)
      let rec eval (env,expr) = match expr with
         NUM i -> i
        |PLUS(e1,e2) -> eval(env,e1) + eval(env,e2)
        |MINUS(e1,e2) -> eval(env,e1) + eval(env,e2)
        |MULT(e1,e2) -> eval(env,e1) * eval(env,e2)
        |DIVIDE(e1,e2) -> eval(env,e1) / eval(env,e2)
        |MAX [] -> 0
        |MAX (e::elist) -> let a = eval(env,e) in 
                                if elist = [] then a 
                                else let b = eval(env,MAX(elist)) in 
                                        if a > b then a else b
        |VAR id -> eval(env,getExpr(env,id))
        |LET(id,e1,e2) -> eval([(id,NUM(eval(env,e1)))]@env,e2)
        
      let int_of_value v = v;
    end
       
