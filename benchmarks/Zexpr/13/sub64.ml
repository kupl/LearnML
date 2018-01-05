(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

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

module Zexpr : ZEXPR = struct
    
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
    
    let emptyEnv = []
    let int_of_value v = v
    let rec eval (env, e) = match e with
NUM(n)-> n
|PLUS(e1, e2)-> eval(env, e1)+eval(env, e2)
|MINUS(e1, e2)->eval(env, e1)-eval(env, e2)
|MULT(e1, e2)->eval(env, e1)*eval(env, e2)
|DIVIDE(e1, e2)->eval(env, e1)/eval(env, e2)
|MAX(ellist)-> if(ellist=[]) then 0 else if(List.tl ellist=[]) then eval(env, List.hd ellist) 
else if(eval(env, List.hd ellist)<eval(env, MAX(List.tl ellist))) then eval(env, MAX(List.tl ellist))
else eval(env, List.hd ellist)
|VAR(id1)-> if(env=emptyEnv) then raise (Error "FreeVariable") 
  else if( id1 = fst (List.hd env)) then snd (List.hd env)
  else eval(List.tl env, e)
|LET(id1, exprid, expf)->
  let subenv= (id1, int_of_value(eval(env, exprid)))::env
  in
  eval(subenv, expf)
end