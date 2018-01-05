module type ZEXPR =
sig
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
type environment
type value
val emptyEnv: environment
val eval: environment * expr -> value
end

module Zexpr: ZEXPR =
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
type environment = (string*expr) list
type value = int
let emptyEnv = []
let rec eval (env,ex)= 
	let rec f_max lst out =
		match lst with
		|[] -> out
		|hd::tl -> if (eval (env,hd))>out then (f_max tl (eval (env,hd))) else (f_max tl out)
	in
	let rec f_var envir var =
		match envir with
		|[] -> raise (Error "varialbe not defined")
		|(vr,vl)::tl -> if vr = var then vl else (f_var tl var)
	in
	match ex with
	|NUM a -> a
	|PLUS(x,y) -> (eval (env,x))+(eval (env,y))
	|MINUS(x,y) -> (eval (env,x))-(eval (env, y))
	|MULT(x,y) -> (eval (env, x))*(eval (env, y))
	|DIVIDE(x,y) -> (eval (env, x))/(eval (env, y))
	|MAX lst -> (f_max lst 0)
	|VAR a -> (eval (env, (f_var env a)))
	|LET(v,e1,e2) -> (eval (((v,e1)::env), e2))
end
