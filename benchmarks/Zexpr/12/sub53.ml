(* Name: Yoon Jae Nam (2012-81338)
   Organization: Seoul National University
   Class: Programming Language (4190.310)
   Assignment: 2
   Problem: 5: Expression *)

(* 1. Provided declarations / definitions *)
module type ZEXPR = sig
	exception Error of string
	type id = string
	type expr = 
		  NUM of int
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
	val eval: environment * expr -> value
end

(* 2. My code *)
let rec getMax : int list -> int = fun l -> (
	match l with
	| [] -> 0
	| h::[] -> h
	| h::t -> (
		let t_max = getMax(t) in (
			if h > t_max then h else t_max
		)
	)
)

let rec getIndexHelper : string * string list * int -> int = (
	fun(str, str_list, index) -> (
		match str_list with
		| [] -> -1
		| h::t -> (
			if h = str then index
			else (
				getIndexHelper(str, t, index + 1)
			)
		)
	)
)

let getIndex : string * string list -> int = fun (str, str_list) -> (
	getIndexHelper(str, str_list, 0)
)

module Zexpr : ZEXPR = struct
	exception Error of string
	type id = string
	type expr = 
		  NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr
		| MULT of expr * expr
		| DIVIDE of expr * expr
		| MAX of expr list
		| VAR of id
		| LET of id * expr * expr
	type environment = (string * int) list
	type value = int
	let emptyEnv : environment = []
	let rec eval: environment * expr -> value = fun (env, exp) -> (
		let env_id_list = List.map (fun (str, num) -> str) env in
		let env_value_list = List.map (fun (str, num) -> num) env in (
			let result : int = (
				match exp with
				| NUM(i) -> i
				| PLUS(exp1, exp2) -> eval(env, exp1) + eval(env, exp2)
				| MINUS(exp1, exp2) -> eval(env, exp1) - eval(env, exp2)
				| MULT(exp1, exp2) -> eval(env, exp1) * eval(env, exp2)
				| DIVIDE(exp1, exp2) -> eval(env, exp1) / eval(env, exp2)
				| MAX(exp_list) -> (
					let addEnv : expr -> (environment * expr) = (
						fun exp -> (env, exp)
					) in (
						let exp_list_with_env = List.map addEnv exp_list in
						let val_list = List.map eval exp_list_with_env in (
							getMax(val_list)
						)
					)
				)
				| VAR(var_id) -> (
					let index = getIndex(var_id, env_id_list) in (
						if index = -1
						then raise (Error("Variable " ^ var_id ^ " is undefined."))
						else (
							List.nth env_value_list index
						)
					)
				)
				| LET(var_id, var_exp, body_exp) -> (
					let var_value = eval(env, var_exp) in
					let new_env = (var_id, var_value)::env in (
						eval(new_env, body_exp)
					)
				)
			) in (
				print_int result;
				result
			)
		)
	)
end

(* 3. Test code *)
(*
let testRunner (test_name, exp, expected, error_expected) = (
	print_endline "==============================================";
	print_endline test_name;
	let empty_env = Zexpr.emptyEnv in (
		if error_expected then (
			try (
				let _ = Zexpr.eval(empty_env, exp) in (
					print_endline "                                          BAD!! ERROR WAS EXPECTED";
					raise (Zexpr.Error "WRONG RESULT")
				)
			) with
				| Zexpr.Error(msg) -> (
					print_endline "                                          GOOD! Error was caught :D";
					Printf.printf "Error message: %s\n" msg
				)
				| _ -> ()
		) else (
			let actual = Zexpr.eval(empty_env, exp) in (
				if actual = expected then (
					print_endline "                                          GOOD!";
					Printf.printf "Expected = %d = actual\n" actual
				) else (
					print_endline "                                          BAD!";
					Printf.printf "Expected = %d != actual = %d\n" expected actual
				)
			)
		)
	)
)

let test = (
	let exp = Zexpr.LET("x", Zexpr.NUM(1),
		Zexpr.PLUS (
			Zexpr.LET("x", Zexpr.NUM(2),
				Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "x")
			), (* 4 *)
			Zexpr.VAR "x"
		)
	) in (* 5 *)
	let expected = 5 in
	let error_expected = false in
	let test_name = "test1" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.LET("x", Zexpr.NUM(1), Zexpr.PLUS (Zexpr.LET("y", Zexpr.NUM(2), Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "y")),Zexpr.VAR "x")) in
	let expected = 4 in
	let error_expected = false in
	let test_name = "test2" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.LET("x", Zexpr.NUM(1),Zexpr.PLUS (Zexpr.LET("y", Zexpr.NUM(2), Zexpr.PLUS(Zexpr.VAR "y", Zexpr.VAR "x")),Zexpr.VAR "y")) in
	let expected = 0 in
	let error_expected = true in
	let test_name = "test3" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.NUM(15)
	in
	let expected = 15 in
	let error_expected = false in
	let test_name = "test4" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.PLUS(Zexpr.NUM(15),Zexpr.NUM(20))
	in
	let expected = 35 in
	let error_expected = false in
	let test_name = "test5" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.MINUS(Zexpr.NUM(15),Zexpr.NUM(20))
	in
	let expected = -5 in
	let error_expected = false in
	let test_name = "test6" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.MULT(Zexpr.NUM(15),Zexpr.NUM(20))
	in
	let expected = 300 in
	let error_expected = false in
	let test_name = "test7" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.DIVIDE(Zexpr.NUM(30),Zexpr.NUM(2))
	in
	let expected = 15 in
	let error_expected = false in
	let test_name = "test8" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.MAX([Zexpr.NUM(30);Zexpr.NUM(2);Zexpr.NUM(-1);Zexpr.NUM(30)])
	in
	let expected = 30 in
	let error_expected = false in
	let test_name = "test9" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.MAX([])
	in
	let expected = 0 in
	let error_expected = false in
	let test_name = "test10" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.LET("a",Zexpr.NUM(5),
		Zexpr.PLUS(Zexpr.VAR("a"),Zexpr.NUM(-1))
	) in
	let expected = 4 in
	let error_expected = false in
	let test_name = "test11" in
	testRunner(test_name, exp, expected, error_expected)
)
let test = (
	let exp = Zexpr.LET("a",Zexpr.NUM(5),
		Zexpr.PLUS(
			Zexpr.VAR("a"),
			Zexpr.LET("a",Zexpr.NUM(100),
				Zexpr.MULT(Zexpr.VAR("a"),Zexpr.NUM(-2))
			)
		)
	) in
	let expected = -195 in
	let error_expected = false in
	let test_name = "test12" in
	testRunner(test_name, exp, expected, error_expected)
)
*)