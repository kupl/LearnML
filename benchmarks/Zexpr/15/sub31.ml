(* hw2ex7 *)

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

  val print_value : value -> int
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
  let rec eval (env, expr) = 
    match expr with
      | NUM i -> i
      | PLUS (e1, e2) -> (eval (env,e1)) + (eval (env,e2))
      | MINUS (e1, e2) -> (eval (env,e1)) - (eval (env,e2))
      | MULT (e1, e2) -> (eval (env,e1)) * (eval (env,e2))
      | DIVIDE (e1, e2) -> (eval (env,e1)) / (eval (env,e2))
      | MAX elist -> (* should be fixed *)
          let rec findMax (l: int list) (v: int) : int = 
            match l with
              | [] -> v
              | hd::tl -> if hd > v then (findMax tl hd) else (findMax tl v) in
          let evalmap = fun e -> eval (env,e) in
            if elist = [] then 0
            else findMax (List.map evalmap elist) (List.hd (List.map evalmap elist))
      | VAR id -> 
          let rec findIdValue ((i,env) : id * environment) = 
            match env with
              | [] -> raise (Error "FreeVariable")
              | hd::tl ->
                  if (fst hd) = id then (snd hd)
                  else findIdValue (i, tl)
          in findIdValue (id, env)
      | LET (id,e1,e2) -> eval ((id, eval(env,e1))::env, e2)

  (*environment * expr -> value*) 

  let print_value : value -> int = 
    fun v -> v


end 




(* testcase

  let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.NUM 1)));; 

  let xpx = Zexpr.PLUS((Zexpr.VAR "x"),(Zexpr.VAR "x")) in 
  let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS (Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x")))) in 
  Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, e1));; 

  let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [])));; 

  let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)])));;

*)
