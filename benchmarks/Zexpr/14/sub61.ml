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
  val int_of_value : value -> int 
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
 type environment = (id*expr) list
 type value = int

 let emptyEnv = []
 let rec  eval (env, exp) =
   match exp with
   |NUM n -> n
   |PLUS (a,b) -> eval(env,a) + eval(env,b)
   |MINUS (a,b) -> eval (env, a) - eval (env,b)
   |MULT (a,b) -> eval(env, a) * eval (env, b)
   |DIVIDE (a,b) -> eval(env,a)/eval(env,a)
   |MAX [expre] -> eval (env, expre)
   |MAX elist -> eval(env, MAX(List.append [max (List.hd elist) (List.hd (List.tl
   elist))] (List.tl (List.tl elist))))
   |LET (varId, varValue, formula) ->
       if ((List.mem_assoc varId env)=true ) then eval((List.append (List.remove_assoc varId env) [(varId, varValue)]), formula)
       else
         eval((List.append env [(varId, varValue)]), formula)
  |VAR vId ->
      try
        eval(env, (List.assoc vId env))
      with Not_found -> raise (Error ("FreeVariable"))
 let int_of_value v = v
end

