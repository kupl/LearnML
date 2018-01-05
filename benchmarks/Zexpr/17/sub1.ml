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
  let emptyEnv : environment = []
  let rec evalvar (env,i) : value =
  match env,i with
    | [] ,a-> raise (Error "FreeVariable")
    | h::t,a ->
    if fst h = i then
        snd h
    else
       evalvar (t,i)
let rec eval ((env, exp) : (environment * expr)) : value =
  match env,exp with
    | env, NUM i -> i
    | env,PLUS (a, b) -> (eval (env, a)) + (eval (env, b))
    | env,MINUS (a, b) -> (eval (env, a)) - (eval (env, b))
    | env,MULT (a, b) -> (eval (env, a)) * (eval (env, b))
    | env,DIVIDE (a, b) -> (eval (env, a)) / (eval (env, b))
    | env,MAX l -> searchmax(env,-999000,l)
    | env,VAR i -> evalvar (env,i)
    | env,LET (i, b, s) -> eval (((i, (eval (env, b)))::env), s)
  and searchmax(a,b,c)=
    match a,b,c with
   |env,tmax,[] -> 0
   |env,tmax,h::[]-> if tmax<eval(env,h) then eval(env,h) else tmax 
   |env,tmax,h::lst->
   let t =eval(env,h) in
   if tmax<t then searchmax(env,t,lst) else searchmax(env,tmax,lst)
    let print_value (v : value) : unit =   
      print_int (v)
end

