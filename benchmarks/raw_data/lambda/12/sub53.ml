(*
Name: Yoon Jae Nam (2012-81338)
Organization: Seoul National University
Class: Programming Language (4190.310)
Problem: 8 (lambda)
*)

(* Provided type declarations *)
type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string
(* End of provided type declarations *)

(* varExists: function that checks if (arg1 : var)
exists in (arg2 : var list) *)
let rec varExists : var * var list -> bool = 
	fun (var_foo, var_list) ->
		match var_list with
		[] -> false
		| h::t -> 
			(if var_foo = h then true
			else (varExists (var_foo, t)))
(* end of varExists *)
		
(* addToNameList: adds (arg1 : var) to (arg2 : var list) if 
arg1 doesn't already appear in arg2 *)
let addToNameList : var * var list -> var list = 
	fun (var_foo, var_list) ->
		if (varExists (var_foo, var_list)) then 
		(* alreay exists; don't have to add *)
			var_list
		else (* need to add var_foo to var_list *) 
			var_foo::var_list
(* end of addToNameList *)

(* checkRec: helper function that has another argument of type 
(var list). arg2 is the list of vars that a station can have *)
let rec checkRec : lambda * var list -> bool = 
	fun (lambda_foo, var_list) ->
		match lambda_foo with
		V(var_bar) -> 
			(* true iff the var of the station exists in var_list *)
			(varExists(var_bar, var_list))
		| P(var_bar, lambda_bar) -> 
			(* true iff the child lambda is correct, after acknowledging 
			its var as a possible station var *)
			(checkRec(lambda_bar, addToNameList(var_bar, var_list)))
		| C(lambda_left, lambda_right) ->  
			(* true iff both lambdas are correct *)
			(checkRec(lambda_left, var_list) 
			&& checkRec(lambda_right, var_list))
(* end of checkRec *)

(* check function that checks for the correctness of a lambda.
Definition of the correctness of a lambda (iff): The var of the station
(id in V(id)) only appears in the area corresponding to that var 
(m in P(id, m)) *)
let check: lambda -> bool = 
	fun lambda_foo -> checkRec(lambda_foo, [])

(* Below: for testing *)
(*
let printBool(bool_foo) =
	if bool_foo = true then print_string "true"
	else print_string "false"		

let test_runner (test_var, test_lambda, expected) =
	let actual = check(test_lambda) in
		print_endline ("---------------");
		if actual = expected then
			(print_string ("Good (" ^ test_var ^ "): ");
			printBool (actual);
			print_newline ()
			)
		else
			(print_endline ("***BAD (" ^ test_var ^ ")");
			print_string "Expected: ";
			printBool(expected);
			print_newline ();
			print_string "Actual: ";
			printBool(actual);
			print_newline ();
			raise (Invalid_argument "test_runner"))

let test1 =
	let test_lambda = P("a", V "a") in
	let expected = true in
	test_runner("test1", test_lambda, expected)
	
let test2 =
	let test_lambda = P("a", P("a", V "a")) in
	let expected = true in
	test_runner("test2", test_lambda, expected)
	
let test3 =
	let test_lambda = P("a", P("b", C(V "a", V "b"))) in
	let expected = true in
	test_runner("test3", test_lambda, expected)
	
let test4 =
	let test_lambda = P("a", C(V "a", P("b", V "a"))) in
	let expected = true in
	test_runner("test4", test_lambda, expected)
	
let test5 =
	let test_lambda = P("a", V "b") in
	let expected = false in
	test_runner("test5", test_lambda, expected)
	
let test6 =
	let test_lambda = P("a", C(V "a", P("b", V "c"))) in
	let expected = false in
	test_runner("test6", test_lambda, expected)
	
let test7 =
	let test_lambda = P("a", P("b", C(V "a", V "c"))) in
	let expected = false in
	test_runner("test7", test_lambda, expected)

let test8 =
	let test_lambda = V "a" in
	let expected = false in
	test_runner("test8", test_lambda, expected)

let test9 =
	let test_lambda = C(V "a", V "b") in
	let expected = false in
	test_runner("test9", test_lambda, expected)

let test10 =
	let test_lambda = P(("c"), C(P("b", P("a", V "b")), P("c", V "b"))) in
	let expected = false in (* because of V "b" at the end *)
	test_runner("test10", test_lambda, expected)

let test11 =
	let test_lambda = P("b", P("c", C(P("b", P("a", V "b")), P("c", V "b")))) in
	let expected = true in
	test_runner("test11", test_lambda, expected)
(* end of test code *)
*)