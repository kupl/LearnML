module type ZEXPR = 
sig
  exception Error of string
  type id = string
  type expr = 
    |NUM of int
    |PLUS of expr * expr
    |MINUS of expr * expr
    |MULT of expr * expr
    |DIVIDE of expr * expr
    |MAX of expr list
    |VAR of id
    |LET of id * expr * expr
  type environment
  type value
  val emptyEnv : environment
  val eval : environment * expr -> value
  val print_value : value -> unit
end

module Zexpr = 
struct
  exception Error of string
  type id = string
  type expr = 
    |NUM of int
    |PLUS of expr * expr
    |MINUS of expr * expr
    |MULT of expr * expr
    |DIVIDE of expr * expr
    |MAX of expr list
    |VAR of id
    |LET of id * expr * expr
  type environment = (string * expr) list
  type value = int 
  let emptyEnv = []
  let rec eval ((env : environment), (e:expr)) = 
    match e with
    |NUM n -> n
    |PLUS (a, b) ->eval(env,a) + eval(env,b)
    |MINUS (a, b) ->eval(env,a) - eval(env,b)
    |MULT (a, b) ->eval(env,a) * eval(env,b)
    |DIVIDE (a,b) ->eval(env,a) / eval(env,b)
    |MAX lst ->
      let rec eton (env,lst) = 
        match lst with 
          |[] -> []
          |h::t -> eval(env,h)::(eton (env,t)) 
      in
      let rec find (n, lst) = match lst with
        |[] -> n
        |h::t -> if(h>n) then find(h,t) else find(n,t)
      in
      let max (env,lst) = match lst with
        |[] -> 0
        |h::t -> find(eval(env,h), eton (env,t)) 
      in
      max (env,lst)
    |VAR s ->
      let rec searchenv ((env:environment), (s:string)) = match env with
        |[] -> raise(Error "FreeVariable")
        |h::tenv -> if(fst h = s)then snd h else searchenv(tenv, s)
      in
      eval(env, searchenv(env, s))
    |LET (s, a, b) -> eval((s,a)::env,b)


  let print_value v = print_int(v);print_newline();
end

