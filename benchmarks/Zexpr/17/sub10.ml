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
  val emptyEnv: environment
  val eval: environment * expr -> value
end

module Zexpr =
  struct
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
  type value  = int    
  type environment =  (id*int) list
  let emptyEnv: environment = []
  let rec eval (env, exp): value =
    match exp with
    |NUM(i) -> i
    |PLUS(e1, e2)->eval(env, e1) + eval(env,e2)
    |MINUS(e1, e2)->eval(env,e1) - eval(env,e2)
    |MULT(e1,e2) ->eval(env,e1) * eval(env,e2)
    |DIVIDE(e1,e2) -> eval(env,e1) / eval(env,e2)
    |MAX([])-> 0
    |MAX(hh::tt)->(
      let rec maximum ((now:int),(l:expr list)):value =
        match l with
        |[] -> now
        |h::t ->(
          let valh = eval(env, h) 
          in if(now<valh)
          then maximum(valh, t)
          else maximum(now, t)
        )
      in maximum(eval(env,hh),tt)
    )
    |VAR(id) -> (try List.assoc id env with _ -> raise (Error "no val"))
    |LET(id, e1, e2)  -> (
      eval((id, (eval (env, e1)))::(List.remove_assoc id env), e2)      
    )

end
