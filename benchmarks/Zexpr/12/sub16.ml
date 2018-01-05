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
type value = int
let emptyEnv = []

let my_max = function
[] -> raise (Error "Empty List")
| x::xs -> List.fold_left max x xs

let rec evaluate env exp = match exp with
NUM i -> i
| PLUS (e0, e1) -> (evaluate env e0) + (evaluate env e1)
| MINUS (e0, e1) -> (evaluate env e0) - (evaluate env e1)
| MULT (e0, e1) -> (evaluate env e0) * (evaluate env e1)
| DIVIDE (e0, e1) ->
	if (evaluate env e1) = 0 then raise (Error "Divide By Zero")
				 else (evaluate env e0) / (evaluate env e1)
| MAX [] -> 0
| MAX lst -> my_max (List.map (fun x -> evaluate env x) lst)
| VAR id ->
	if (List.mem_assoc id env) then List.assoc id env
				   else raise (Error "id is not found in the environment")
| LET (id, v, expr) -> let value = evaluate env v in
	if (List.mem_assoc id env) then evaluate ((id, value)::(List.remove_assoc id env))
						 expr
				   else evaluate ((id, value)::env) expr

let eval (env, exp) = let value = (evaluate env exp) in
	print_int value;
	value
end

(* The "open" statement allows a module to be used without being prefixed by the module name. *)
(*
open Zexpr
let x0 = LET ("x", NUM 1,
		PLUS (LET ("x", NUM 2, PLUS (VAR "x", VAR "x")),
			VAR "x"));;
let x1 = LET ("x", NUM 1,
		PLUS (LET ("y", NUM 2, PLUS (VAR "x", VAR "y")),
			VAR "x"));;
let x2 = LET ("x", NUM 1,
	    PLUS (LET ("y", NUM 2, PLUS (VAR "y", VAR "x")),
	    	  VAR "y"));;
*)
