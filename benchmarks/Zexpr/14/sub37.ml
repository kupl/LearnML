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
type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list
| VAR of id
| LET of id * expr * expr
type environment = (id*expr) list
type value= expr
let emptyEnv =[]

let rec evall (env,expr) =
	match expr with 
	| NUM x -> x
	| PLUS (a,b) -> evall(env,a)+evall(env,b)
	| MINUS (a,b) -> evall(env,a)-evall(env,b)
	| MULT (a,b) -> evall(env,a)*evall(env,b)
	| DIVIDE(a,b) -> 
			if (evall(env,b) =0 ) then raise (Error "Div by Zero") else evall(env,a)/evall(env,b)
	| MAX l -> (match l with
			| h::[]-> evall(env,h)
			| h::t->let a= evall(env,h) in
				let b= evall(env,(MAX t)) in 
				if a<b then b else a 
			| [] -> 0 
			)
	| VAR x -> (match env with
			| (idd,b)::t-> if idd= x then evall(env,b) else evall(t,expr)
			| [] -> raise (Error "FreeVariable") 
)
	| LET (x,e1,e2) -> 
			   evall(((x,e1)::env),e2) 
let int_of_value (a:value) : int=
	match a with
	|NUM x -> x
	| _ -> raise (Error "ERROR")
let eval (env,expr) : value=
	NUM (evall (env,expr)) 
	
end	
