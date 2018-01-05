module type ZEXPR = sig
exception Error of string
type id = string
type expr = | NUM of int 
| PLUS of expr * expr 
| MINUS of expr * expr 
| MULT  of expr *expr 
| DIVIDE of expr * expr 
| MAX of expr list 
| VAR of id 
| LET of id * expr * expr
type environment
type value
val emptyEnv: environment
val eval : environment * expr -> value
end

module Zexpr : ZEXPR = struct
exception Error of string
type id = string
type expr = | NUM of int 
| PLUS of expr * expr 
| MINUS of expr * expr 
| MULT of expr *expr 
| DIVIDE of expr * expr 
| MAX of expr list 
| VAR of id 
| LET of id * expr * expr

type environment = Env of (id * expr) list
type value = int 

let emptyEnv = Env []

let rec eval (env, e) =
let rec eval1 (env, e) =
	let rec isEnv (env, id)= 
	match env with
	| Env [] -> raise (Error "InvalideEnv")
	| Env (h::t) -> match h with 
		    | (a, NUM b) -> if a = id then b else isEnv (Env t,id)
		    | _ -> raise (Error "Env is Error")
in
	match e with
	| NUM i -> i
	| PLUS (e1, e2) -> eval1 (env, e1) + eval1 (env, e2)
	| MINUS (e1, e2) -> eval1 (env, e1) - eval1 (env, e2)
	| MULT (e1, e2) -> eval1 (env, e1) * eval1 (env, e2)
	| DIVIDE (e1, e2) -> if (eval1 (env, e2)) = 0 then  raise (Error "InvalidDivide")
							else eval1 (env, e1) / eval1 (env, e2)
	| MAX elist -> let f = fun x -> eval1 (env, x) in
			let compare = fun x y -> if x < y then 1 else if x = y then 0 else -1 in 
			List.hd (List.sort compare (List.map f elist))
	| VAR id -> isEnv (env, id)
	| LET (id, e1, e2) -> (match env with
			      			| Env [] -> eval1 (Env ((id, NUM (eval1 (env, e1)))::[]), e2)
							| Env a -> eval1 (Env ((id, NUM (eval1 (env, e1)))::a), e2)
						   )
in
let a = eval1 (env, e) in
let _ = print_int a in
a
end
open Zexpr
