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
  type pair = id * value
  type environment = pair list

  let emptyEnv = []
  let rec eval (env, exp) = match exp with
    | NUM x -> x
    | PLUS (exp1, exp2) -> (eval (env, exp1)) + (eval (env, exp2))
    | MINUS (exp1, exp2) -> (eval (env, exp1)) - (eval (env, exp2))
    | MULT (exp1, exp2) -> (eval (env, exp1)) * (eval (env, exp2))
    | DIVIDE (exp1, exp2) -> (eval (env, exp1)) / (eval (env, exp2))
    | MAX x -> let rec maxmax l = (match l with
      | [] -> 0
      | [x] -> eval(env, x)
      | hd::tl -> let (mx1, mx2) = (eval(env, hd), maxmax tl) in (
          if mx1 > mx2 then mx1 else mx2
        )
      ) in maxmax x
    | VAR name -> (match env with
      | [] -> raise (Error "FreeVariable")
      | hd::tl -> if fst(hd) = name then snd(hd) else eval(tl, VAR name)
      )
    | LET (id, expr1, expr2) -> eval( (id,eval(env,expr1))::env, expr2)

  let print_value x = print_int x
end 
