module type ZEXPR = 
sig
	exception Error of string
	type id = string
	type expr = NUM of int
						| PLUS of expr*expr
						| MINUS of expr*expr
						| MULT of expr*expr
						| DIVIDE of expr*expr
						| MAX of expr list
						| VAR of id
						| LET of id*expr*expr

	type environment
	type value

	val emptyEnv: environment
	val eval: environment*expr -> value

	val print_value: value -> unit

end


module Zexpr: ZEXPR=
struct
	(* Implement this module *)
	exception Error of string
	(* should I write all the defined type? *)
	type id = string
	type expr = NUM of int
						| PLUS of expr*expr
						| MINUS of expr*expr
						| MULT of expr*expr
						| DIVIDE of expr*expr
						| MAX of expr list
						| VAR of id
						| LET of id*expr*expr

	type environment = (id*value) list
	and value = int

	let emptyEnv = []

	let eval (env, expr) =  (* : environment * expr -> value *)

		let rec findmax acc = function
			| [] -> acc
			| hd::tl -> if hd > acc then findmax hd tl else findmax acc tl
		in

		let rec get_env_value env var =
			match env with
			| [] -> raise (Error "FreeVariable")
			| hd::tl -> let (x,v) = List.hd env in if x=var then v else get_env_value tl var
		in

		let rec aux env = function (* : environment -> expr -> int *)
			| NUM i 					-> i
			| PLUS( e1, e2 ) 	-> aux env e1 + aux env e2
			| MINUS( e1, e2 ) -> aux env e1 - aux env e2
			| MULT( e1, e2 ) 	-> aux env e1 * aux env e2
			| DIVIDE( e1, e2 )-> aux env e1 / aux env e2
			| MAX l ->
				( match l with
					| [] 			-> 0
					| _ as l 	-> (findmax (aux env (List.hd l)) (List.map (aux env) l) )
				)
			| VAR id -> get_env_value env id
			| LET( id, e1, e2 ) -> aux ( (id, (aux env e1) )::env) e2
		in
		aux env expr		

	let print_value v  = print_int v; print_string "\n"

end


(*
open Zexpr

let testquery = LET( "x", NUM 1, PLUS( LET("x", NUM 2, PLUS( VAR "x", VAR "x" ) ), VAR "x" ) )
let testq2 = LET( "x", NUM 1, PLUS( LET( "y", NUM 2, PLUS( VAR "x", VAR "y") ), VAR "x" ) )
let testq3 = LET( "x", NUM 1, PLUS( LET( "y", NUM 2, PLUS( VAR "y", VAR "x") ), VAR "y" ) )
let testq4 = MAX [NUM 1; NUM 3; NUM 2]

let _= print_value (eval (emptyEnv, testquery ))
let _= print_value (eval (emptyEnv, testq2))
let _= print_value (eval (emptyEnv, testq4))
*)
