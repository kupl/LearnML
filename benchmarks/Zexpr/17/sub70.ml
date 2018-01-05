
(* EXERCISE 7 *)
module type ZEXPR =
sig
	exception ERROR of string
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
	val emptyEnv : environment
	val eval : environment * expr -> value
	
	val print_value : value -> unit
end

module Zexpr =
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
	type value = int
	type environment = (id * value) list
	let emptyEnv : environment = []
	let rec eval : environment * expr -> value = fun (env_in, exp_in) ->
		let rec probe_env : (environment * id ) -> value = fun (env_in, id_in) ->
			match env_in with 
			|h :: t -> if (fst h) = id_in then snd h
				else probe_env (t,id_in)
			|[] -> raise (Error "FreeVariable")
		in
		let rec probe_valuemax : expr list -> value = fun lexp_in ->
			match lexp_in with
			| [] -> 0
			| h::[] -> eval(env_in,h)
			|h :: t -> if eval(env_in, h) >= probe_valuemax (t) then eval(env_in, h) 
					else probe_valuemax (t)
		in
		match exp_in with
		| NUM (n_in) -> n_in
		| PLUS (sub1exp_in, sub2exp_in) -> eval (env_in, sub1exp_in) + eval (env_in , sub2exp_in)
		| MINUS (sub1exp_in , sub2exp_in) -> eval (env_in, sub1exp_in) - eval (env_in, sub2exp_in)
		| MULT (sub1exp_in, sub2exp_in) -> eval (env_in, sub1exp_in) * eval (env_in , sub2exp_in)
		| DIVIDE (sub1exp_in, sub2exp_in) -> eval (env_in, sub1exp_in) / eval (env_in, sub2exp_in)
		| MAX (sublist_exp_in) -> probe_valuemax(sublist_exp_in)
		| VAR (id_in) -> probe_env (env_in, id_in)
		| LET (id_in, sub1exp_in, sub2exp_in) -> eval ((id_in, eval(env_in,sub1exp_in)):: env_in , sub2exp_in)
	
	let print_value : value -> unit = fun input ->
		print_endline(string_of_int input) (* value is int *)
end;;

(*
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.NUM 1)), print_string "Case 1 : 1 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS(Zexpr.LET("x", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "x")), Zexpr.VAR "x")))), print_string "Case 2 : 5 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX []))), print_string "Case 3 : 0 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)]))), print_string "Case 4 : -1 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.DIVIDE(Zexpr.NUM 3, Zexpr.NUM 2)))), print_string "Case 5 : 1 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.PLUS(Zexpr.NUM 7, Zexpr.NUM 9)))), print_string "Case 6 : 16 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MINUS(Zexpr.NUM 7, Zexpr.NUM 9)))), print_string "Case 7 : -2 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MULT(Zexpr.NUM 7, Zexpr.NUM 9)))), print_string "Case 8 : 63 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS(Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "y")), Zexpr.VAR "x")))), print_string "Case 9 : 4 vs " 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.LET("x", Zexpr.NUM 1, Zexpr.LET("y", Zexpr.NUM 2, Zexpr.LET("z", Zexpr.NUM (-1), Zexpr.MAX[Zexpr.VAR "x"; Zexpr.VAR "y" ; Zexpr.VAR "z"])))))), print_string "Case 10 : 2 vs " 
let _ = try Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.LET("x", Zexpr.NUM 1, Zexpr.LET("y", Zexpr.NUM 2, Zexpr.LET("z", Zexpr.NUM (-1), Zexpr.MAX[Zexpr.VAR "x"; Zexpr.VAR "y" ; Zexpr.VAR "z"; Zexpr.VAR "a"])))))) with Zexpr.Error x -> 
if (x = "FreeVariable") then print_endline("Error Case 1 : Pass") 
else print_endline("Error Case 1 : Failure") 
let _ = try Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS(Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "y")), Zexpr.VAR "y")))) with Zexpr.Error x -> 
if (x = "FreeVariable") then print_endline("Error Case 2 : Pass") 
else print_endline("Error Case 2 : Failure")
*)
