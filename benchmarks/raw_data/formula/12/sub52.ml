(*
Name: Yoon Jae Nam (2012-81338)
Organization: Seoul National University
Class: Programming Language (4190.310)
Problem: 6: true or false
*)

(* Provided type declarations *)
type formula = True
			| False
			| Not of formula
			| AndAlso of formula * formula
			| OrElse of formula * formula
			| Imply of formula * formula
			| Equal of exp * exp
and exp = Num of int
		| Plus of exp * exp
		| Minus of exp * exp
(* end of provided types *)

(* evalExpr evalutes an exp to an integer *)
let rec evalExpr : exp -> int = fun exp_foo ->
	match exp_foo with
	Num (num) -> num
	| Plus (e_left, e_right) ->
		((evalExpr e_left) + (evalExpr e_right))
	| Minus (e_left, e_right) ->
		((evalExpr e_left) - (evalExpr e_right))
(* end of evalExpr *)

(* eval function that evaluates a formula to a boolean value *)
let rec eval : formula -> bool = fun formula_foo ->
	match formula_foo with
	True -> true
	| False -> false
	| Not formula_bar -> not (eval formula_bar)
	| AndAlso (f_left, f_right) ->
		(let bool_left = (eval f_left) in
		let bool_right = (eval f_right) in
			match (bool_left, bool_right) with
			(true, true) -> true
			| (_, _) -> false)
	| OrElse (f_left, f_right) ->
		(let bool_left = (eval f_left) in
		let bool_right = (eval f_right) in
			match (bool_left, bool_right) with
			(false, false) -> false
			| (_, _) -> true)
	| Imply (f_left, f_right) ->
		(let bool_left = (eval f_left) in
		let bool_right = (eval f_right) in
			match (bool_left, bool_right) with
			(false, _) -> true
			| (true, true) -> true
			| (true, false) -> false)
	| Equal (e_left, e_right) ->
		(let int_left = (evalExpr e_left) in
		let int_right = (evalExpr e_right) in
		if (int_left = int_right) then true
		else false)
	
(* Below: for testing *)
(*
let printBool (bool_foo) =
	if bool_foo = true then print_string "true"
	else print_string "false"		

let test_runner (test_name, test_formula, expected) =
	let actual = eval(test_formula) in
		print_endline ("---------------");
		if actual = expected then
			(print_string ("Good (" ^ test_name ^ "): ");
			printBool (actual);
			print_newline ()
			)
		else
			(print_endline ("***BAD (" ^ test_name ^ ")");
			print_string "Expected: ";
			printBool(expected);
			print_newline ();
			print_string "Actual: ";
			printBool(actual);
			print_newline ();
			raise (Invalid_argument "test_runner"))

let test1 =
	let test_formula = True in
	let expected = true in
	test_runner("True", test_formula, expected)

let test2 =
	let test_formula = False in
	let expected = false in
	test_runner("False", test_formula, expected)

let test3 =
	let test_formula = Not True in
	let expected = false in
	test_runner("Not True", test_formula, expected)

let test4 =
	let test_formula = Not False in
	let expected = true in
	test_runner("Not False", test_formula, expected)

let test5 =
	let test_formula = Not (Not True) in
	let expected = true in
	test_runner("Not (Not True)", test_formula, expected)

let test6 =
	let test_formula = AndAlso (True, True) in
	let expected = true in
	test_runner("AndAlso (True, True)", test_formula, expected)

let test7 =
	let test_formula = AndAlso (True, False) in
	let expected = false in
	test_runner("AndAlso (True, False)", test_formula, expected)

let test8 =
	let test_formula = AndAlso (False, True) in
	let expected = false in
	test_runner("AndAlso (False, True)", test_formula, expected)

let test9 =
	let test_formula = AndAlso (False, False) in
	let expected = false in
	test_runner("AndAlso (False, False)", test_formula, expected)

let test10 =
	let test_formula = OrElse (True, True) in
	let expected = true in
	test_runner("OrElse (True, True)", test_formula, expected)

let test11 =
	let test_formula = OrElse (False, True) in
	let expected = true in
	test_runner("OrElse (False, True)", test_formula, expected)

let test12 =
	let test_formula = OrElse (True, False) in
	let expected = true in
	test_runner("OrElse (True, False)", test_formula, expected)

let test13 =
	let test_formula = OrElse (False, False) in
	let expected = false in
	test_runner("OrElse (False, False)", test_formula, expected)

let test14 =
	let test_formula = Imply (True, True) in
	let expected = true in
	test_runner("Imply (True, True)", test_formula, expected)

let test15 =
	let test_formula = Imply (True, False) in
	let expected = false in
	test_runner("Imply (True, False)", test_formula, expected)

let test16 =
	let test_formula = Imply (False, True) in
	let expected = true in
	test_runner("Imply (False, True)", test_formula, expected)

let test17 =
	let test_formula = Imply (False, False) in
	let expected = true in
	test_runner("Imply (False, False)", test_formula, expected)

let test18 =
	let test_formula = Imply (AndAlso(True, False), Not (Not True)) in
	let expected = true in
	test_runner("Imply (AndAlso(True, False), Not (Not True))", test_formula, expected)

let test19 =
	let test_formula = Imply (OrElse(True, False), Not (Not False)) in
	let expected = false in
	test_runner("Imply (OrElse(True, False), Not (Not False))", test_formula, expected)

let test20 =
	let test_formula = Equal((Num 5), (Num 6)) in
	let expected = true in
	test_runner("Equal((Num 5), (Num 6))", test_formula, expected)

let test21 =
	let test_formula = Equal((Num 6), (Num 6)) in
	let expected = false in
	test_runner("Equal((Num 6), (Num 6))", test_formula, expected)

let test22 =
	let test_formula = Equal((Num 7), (Num 6)) in
	let expected = false in
	test_runner("Equal((Num 7), (Num 6))", test_formula, expected)

let test23 =
	let test_formula = Equal(Plus((Num 5), (Num 6)), (Num 10)) in
	let expected = false in
	test_runner("Equal(Plus((Num 5), (Num 6)), (Num 10))", test_formula, expected)

let test24 =
	let test_formula = Equal(Plus((Num 5), (Num 6)), (Num 11)) in
	let expected = false in
	test_runner("Equal(Plus((Num 5), (Num 6)), (Num 11))", test_formula, expected)

let test25 =
	let test_formula = Equal(Plus((Num 5), (Num 6)), (Num 12)) in
	let expected = true in
	test_runner("Equal(Plus((Num 5), (Num 6)), (Num 12))", test_formula, expected)
(* end of test code *)
*)