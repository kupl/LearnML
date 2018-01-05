open List

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
end;; 

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
    type environment = (id*expr) list
    type value = int

    let emptyEnv : environment = []
    let rec eval : environment * expr -> value = fun (env,expr) -> (
       match expr with
       NUM i-> i
       | PLUS(e1,e2) -> eval(env,e1) + eval(env,e2) 
       | MINUS(e1,e2) -> eval(env,e1) - eval(env,e2) 
       | MULT(e1,e2) -> eval(env,e1) * eval(env,e2) 
       | DIVIDE(e1,e2) -> eval(env,e1) / eval(env,e2) 
       | MAX(eli) -> (
            match eli with
            [] -> 0
            | [only_one] -> eval(env,only_one)
            | fst::snd::li -> (
                let fstv = eval(env,fst) in
                let sndv = eval(env,snd) in
                let maxe = max fstv sndv in
                if( (List.length li)=0 ) then (maxe) else (
                    eval(env,MAX( (NUM maxe)::li))
                )
            )
       )
       | VAR(str) -> (
           match env with
           [] -> raise(Error "FreeVariable")
           | (hid,hexpr)::tlenv -> (
                if(hid=str) then (eval(env,hexpr)) else (eval(tlenv,expr))
           )
       )
       | LET(i,e1,e2) -> (
           let e1v = eval(env, e1) in
           eval((i,NUM e1v)::env, e2)
           )
    )
    let print_value : value -> unit = fun v -> (
        print_int v
    )
end;;
