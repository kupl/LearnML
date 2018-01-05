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

module Zexpr: ZEXPR = 
  struct
    exception Error of string
    exception FreeVariable
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

    type value = 
      | RESULT of int
      | VAL_PLUS of value * value
      | VAL_MINUS of value * value
      | VAL_MULT of value * value
      | VAL_DIVIDE of value * value 

    type environment = LIST of (id * expr) list
    let emptyEnv = LIST []

    let rec print_value value = 
      match value with
      |RESULT a -> a
      |VAL_PLUS (a, b) -> (int_of_value a) + (int_of_value b)
      |VAL_MINUS (a, b) -> (int_of_value a) - (int_of_value b)
      |VAL_MULT (a, b) -> (int_of_value a) * (int_of_value b)
      |VAL_DIVIDE (a, b) -> (int_of_value a) / (int_of_value b)
    
    let enter_env (id, expr, env) =  
	match env with
	|LIST a -> LIST ((id, expr)::a)

    let rec change(env, expr) = 
      match expr with
      |NUM a -> RESULT a
      |PLUS (a, b) -> VAL_PLUS (change (env, a), change (env, b))
      |MINUS (a, b) -> VAL_MINUS (change (env, a), change (env, b))
      |MULT (a, b) -> VAL_MULT (change (env, a), change (env, b))
      |DIVIDE (a, b) -> VAL_DIVIDE (change (env, a), change (env, b))
      |_ -> raise (Error "FreeVariable")

    let rec eval_cal (env, expr) =
      match expr with
      |NUM a -> NUM a
      |PLUS (a, b) -> PLUS (eval_cal (env, a), eval_cal (env, b))
      |MINUS (a, b) -> MINUS (eval_cal (env, a), eval_cal (env, b))
      |MULT (a, b) -> MULT (eval_cal (env, a), eval_cal (env, b))
      |DIVIDE (a, b) -> DIVIDE (eval_cal (env, a), eval_cal (env, b))
      |MAX lst ->
	  (match lst with
	  |[] -> NUM 0
	  |a::[] -> eval_cal (env, a)
	  |a::b::tail ->
	      (if (int_of_value (change(emptyEnv, eval_cal (env, a))))
		  - (int_of_value (change(emptyEnv, eval_cal (env, b)))) > 0
	      then eval_cal (env, MAX (a::tail))
	      else eval_cal (env, MAX (b::tail))))
      |VAR a -> 
	  (match env with
	  |LIST [] -> raise (Error "FreeVariable")
	  |LIST ((id, exp)::tail) -> 
	      (if id = a then exp
		  else eval_cal(LIST tail, VAR a)))
      |LET (id, exp1, exp2) -> eval_cal(env, eval_cal(enter_env(id, exp1, env), exp2))

    let eval(env, expr) =
      change(emptyEnv, eval_cal(env, expr))
end
