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
  type expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr
            | MULT of expr * expr
            | DIVIDE of expr * expr
            | MAX of expr list
            | VAR of id
            | LET of id * expr * expr

  type environment = (id * int) list

  let env_insert : environment * id * int -> environment = 
   fun (env, str, v) -> (str, v) :: env

  let rec env_return : environment * id -> int = fun (env, str) ->
    match env with
      [] -> raise (Error "FreeVariable")
    | (s,v)::t -> if str=s then v else env_return(t,str)

  type value = int

  let emptyEnv: environment = []

  let rec eval: environment * expr -> value = fun (env, exp) ->
   match exp with
     NUM n -> n
   | PLUS (exp1, exp2) -> eval(env, exp1) + eval(env, exp2)
   | MINUS (exp1, exp2) -> eval(env, exp1) - eval(env, exp2)
   | MULT (exp1, exp2) -> eval(env, exp1) * eval(env, exp2)
   | DIVIDE (exp1, exp2) -> eval(env, exp1) / eval(env,exp2)
   | MAX exp_list ->
     (match exp_list with
       [] -> 0
     | h::t -> if t=[] then eval(env,h)
               else if (eval(env,h) > eval(env,MAX t)) then eval(env, h)
               else eval(env,MAX t)
     )

   | VAR str -> env_return(env,str)
   | LET (str,exp1,exp2) -> eval( env_insert(env,str,eval(env,exp1)) ,exp2)


  let print_value : value -> unit = fun num -> print_int num ; print_string "\n"

 end