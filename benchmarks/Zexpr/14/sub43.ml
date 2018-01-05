exception Todo (*foundation done*)
exception FreeVariable
exception CompareError

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

module Zexpr: ZEXPR = 
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

  type environment = string * int
  type value = int

  let emptyEnv : environment = ("",0)
	
	let int_compare ((i1, i2): (value * value)): value =
		if i1 >= i2 then i1 else i2
	
	let rec findMax ((l, max): (value list*value)): value = 
		match l with
		| [] -> max
		| hd::tl -> int_compare (hd, max)

	let rec makeSubEnv ((n, var, val_var): (int * string * int)) = 
		if n = 0 then []
		else (var,val_var)::(makeSubEnv (n-1, var, val_var))

	let rec makeEnv ((n, env): (int*environment)): (environment list) = 
		if n = 0 then []
		else env::(makeEnv ((n-1), env))
		
	let rec substitute ((expr, (var, val_var)): (expr * (string * int))): expr = 
		match expr with
		| NUM i -> expr
		| PLUS (e1, e2) -> PLUS (substitute (e1, (var, val_var)), substitute (e2, (var, val_var)))
		| MINUS (e1, e2) -> MINUS (substitute (e1, (var, val_var)), substitute (e2, (var, val_var)))
		| MULT (e1, e2) -> MULT (substitute (e1, (var, val_var)), substitute (e2, (var, val_var)))
		| DIVIDE (e1, e2) -> DIVIDE (substitute (e1, (var, val_var)), substitute (e2, (var, val_var)))
		| MAX exprlist -> MAX (List.map substitute (List.combine exprlist (makeSubEnv (List.length exprlist, var, val_var))))
		| VAR id -> if id = var then NUM val_var else VAR id
    | LET (id, e1, e2) -> substitute (e2, (id, eval ((var,val_var), e1)))
		
  and eval ((env, expr): (environment * expr)): value =
		match env, expr with
		| ("",0), NUM i -> i
		| ("",0), PLUS (expr1,expr2) -> (eval (env, expr1)) + (eval (env, expr2))
		| ("",0), MINUS (expr1,expr2) -> (eval (env, expr1)) - (eval (env, expr2))
		| ("",0), MULT (expr1,expr2) -> (eval (env, expr1)) * (eval (env, expr2))
		| ("",0), DIVIDE (expr1,expr2) -> (eval (env, expr1)) / (eval (env, expr2))
		| ("",0), MAX exprl -> 
				let vl = List.map eval (List.combine (makeEnv ((List.length exprl), env)) exprl)
				in findMax (vl, List.hd vl)
		| ("",0), VAR x -> raise FreeVariable
		| ("",0), LET (id, expr1,expr2) -> eval ((id,eval (env, expr1)), expr2)
		| (var, val_var), VAR x -> if var = x then val_var else raise (Error "Free Variable")
		| (var, val_var), expr -> eval (("",0), substitute (expr, (var, val_var)))		

  let int_of_value (v: value): int = v
	
end