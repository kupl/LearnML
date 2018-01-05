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
  val eval : environment * expr
  -> value 

  val int_of_value : value ->
    int 
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

    type environment = (id * int) list
    type value = int

    let emptyEnv : environment = []
    
    let rec eval(env, exp) = match exp with
    |NUM i -> i
    |PLUS(a,b) -> eval(env, a) + eval(env, b)
    |MINUS(a,b) -> eval(env, a) - eval(env, b)
    |MULT(a,b) -> eval(env, a) *  eval(env,b)
    |DIVIDE(a,b) -> eval(env, a) / eval(env, b)
    |MAX(a) -> (match a with
      |[] -> 0
      |b::c -> let rec myMax(lst, max) = match lst with
        |[] -> max
        |d::e ->
            let dd = eval(env, d) in
            if(dd > max) 
          then myMax(e, dd)
          else myMax(e, max)
      in
      myMax(c, eval(env, b))) 
    |VAR i -> 
        let rec getValue(lst) = match lst with
        |[] -> raise (Error "FreeVariable")
        |(tid, tval)::tail -> 
            if (i = tid) then tval
            else getValue(tail)
        in
        getValue(env)
    |LET(tid, tval, f) -> eval( (tid, eval(env, tval))::env, f)
         
    
    let int_of_value(a) = a
end

