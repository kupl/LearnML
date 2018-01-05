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
    type environment = (string * int) list
    type value
    val emptyEnv : environment
    val eval : environment * expr -> value
    val int_of_value : value->int
  end
  module Zexpr : ZEXPR =
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
      type environment = (string * int) list
      type value = int
      let emptyEnv = []

      let rec eval (env, ex) = (
        match ex with
        | NUM a -> a
        | PLUS (a,b) -> eval(env,a) + eval(env,b)
        | MINUS (a,b) -> eval(env,a) - eval(env,b)
        | MULT (a,b) -> eval(env,a) *  eval(env,b)
        | DIVIDE (a,b) ->eval(env,a) / eval(env,b)
        | MAX [] -> 0
        | MAX a -> (List.hd (Sort.list (fun a b -> a>b) (maxValue (env, a))))
        | VAR a -> findValue (a, env)
        | LET (a,b,c) -> eval((a,eval(env, b))::env,c)
        )
      and maxValue (env, (ex_list:expr list)) = (
        match ex_list with
        |[] -> []
        |hd::tl -> (eval (env, hd))::maxValue(env, tl)
        )
      and findValue (str, env) =(
        match env with
        |[] -> raise (Error "FreeVariable")
        |hd::tl ->(
          match hd with
          | (a,b) -> (
            if(a = str) then
              b
            else
              findValue (str, tl)
              )    
          )
        ) 
      let int_of_value v = v;
  end 
