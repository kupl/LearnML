(*
Name: Yoon Jae Nam (2012-81338)
Organization: Seoul National University
Class: Programming Language (4190.310)
Problem: 6: true or false
*)

(* Provided type declarations *)
type formula = TRUE
			| FALSE
			| NOT of formula
			| ANDALSO of formula * formula
			| ORELSE of formula * formula
			| IMPLY of formula * formula
			| LESS of expr * expr
and expr = NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr
(* end of provided types *)

(* evalExpr evalutes an expr to an integer *)
let rec evalExpr : expr -> int = fun expr_foo ->
	match expr_foo with
	NUM (num) -> num
	| PLUS (e_left, e_right) ->
		((evalExpr e_left) + (evalExpr e_right))
	| MINUS (e_left, e_right) ->
		((evalExpr e_left) - (evalExpr e_right))
(* end of evalExpr *)

(* eval function that evaluates a formula to a boolean value *)
let rec eval : formula -> bool = fun formula_foo ->
	match formula_foo with
	TRUE -> true
	| FALSE -> false
	| NOT formula_bar -> not (eval formula_bar)
	| ANDALSO (f_left, f_right) ->
		(let bool_left = (eval f_left) in
		let bool_right = (eval f_right) in
			match (bool_left, bool_right) with
			(true, true) -> true
			| (_, _) -> false)
	| ORELSE (f_left, f_right) ->
		(let bool_left = (eval f_left) in
		let bool_right = (eval f_right) in
			match (bool_left, bool_right) with
			(false, false) -> false
			| (_, _) -> true)
	| IMPLY (f_left, f_right) ->
		(let bool_left = (eval f_left) in
		let bool_right = (eval f_right) in
			match (bool_left, bool_right) with
			(false, _) -> true
			| (true, true) -> true
			| (true, false) -> false)
	| LESS (e_left, e_right) ->
		(let int_left = (evalExpr e_left) in
		let int_right = (evalExpr e_right) in
		if (int_left < int_right) then true
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
	let test_formula = TRUE in
	let expected = true in
	test_runner("TRUE", test_formula, expected)

let test2 =
	let test_formula = FALSE in
	let expected = false in
	test_runner("FALSE", test_formula, expected)

let test3 =
	let test_formula = NOT TRUE in
	let expected = false in
	test_runner("NOT TRUE", test_formula, expected)

let test4 =
	let test_formula = NOT FALSE in
	let expected = true in
	test_runner("NOT FALSE", test_formula, expected)

let test5 =
	let test_formula = NOT (NOT TRUE) in
	let expected = true in
	test_runner("NOT (NOT TRUE)", test_formula, expected)

let test6 =
	let test_formula = ANDALSO (TRUE, TRUE) in
	let expected = true in
	test_runner("ANDALSO (TRUE, TRUE)", test_formula, expected)

let test7 =
	let test_formula = ANDALSO (TRUE, FALSE) in
	let expected = false in
	test_runner("ANDALSO (TRUE, FALSE)", test_formula, expected)

let test8 =
	let test_formula = ANDALSO (FALSE, TRUE) in
	let expected = false in
	test_runner("ANDALSO (FALSE, TRUE)", test_formula, expected)

let test9 =
	let test_formula = ANDALSO (FALSE, FALSE) in
	let expected = false in
	test_runner("ANDALSO (FALSE, FALSE)", test_formula, expected)

let test10 =
	let test_formula = ORELSE (TRUE, TRUE) in
	let expected = true in
	test_runner("ORELSE (TRUE, TRUE)", test_formula, expected)

let test11 =
	let test_formula = ORELSE (FALSE, TRUE) in
	let expected = true in
	test_runner("ORELSE (FALSE, TRUE)", test_formula, expected)

let test12 =
	let test_formula = ORELSE (TRUE, FALSE) in
	let expected = true in
	test_runner("ORELSE (TRUE, FALSE)", test_formula, expected)

let test13 =
	let test_formula = ORELSE (FALSE, FALSE) in
	let expected = false in
	test_runner("ORELSE (FALSE, FALSE)", test_formula, expected)

let test14 =
	let test_formula = IMPLY (TRUE, TRUE) in
	let expected = true in
	test_runner("IMPLY (TRUE, TRUE)", test_formula, expected)

let test15 =
	let test_formula = IMPLY (TRUE, FALSE) in
	let expected = false in
	test_runner("IMPLY (TRUE, FALSE)", test_formula, expected)

let test16 =
	let test_formula = IMPLY (FALSE, TRUE) in
	let expected = true in
	test_runner("IMPLY (FALSE, TRUE)", test_formula, expected)

let test17 =
	let test_formula = IMPLY (FALSE, FALSE) in
	let expected = true in
	test_runner("IMPLY (FALSE, FALSE)", test_formula, expected)

let test18 =
	let test_formula = IMPLY (ANDALSO(TRUE, FALSE), NOT (NOT TRUE)) in
	let expected = true in
	test_runner("IMPLY (ANDALSO(TRUE, FALSE), NOT (NOT TRUE))", test_formula, expected)

let test19 =
	let test_formula = IMPLY (ORELSE(TRUE, FALSE), NOT (NOT FALSE)) in
	let expected = false in
	test_runner("IMPLY (ORELSE(TRUE, FALSE), NOT (NOT FALSE))", test_formula, expected)

let test20 =
	let test_formula = LESS((NUM 5), (NUM 6)) in
	let expected = true in
	test_runner("LESS((NUM 5), (NUM 6))", test_formula, expected)

let test21 =
	let test_formula = LESS((NUM 6), (NUM 6)) in
	let expected = false in
	test_runner("LESS((NUM 6), (NUM 6))", test_formula, expected)

let test22 =
	let test_formula = LESS((NUM 7), (NUM 6)) in
	let expected = false in
	test_runner("LESS((NUM 7), (NUM 6))", test_formula, expected)

let test23 =
	let test_formula = LESS(PLUS((NUM 5), (NUM 6)), (NUM 10)) in
	let expected = false in
	test_runner("LESS(PLUS((NUM 5), (NUM 6)), (NUM 10))", test_formula, expected)

let test24 =
	let test_formula = LESS(PLUS((NUM 5), (NUM 6)), (NUM 11)) in
	let expected = false in
	test_runner("LESS(PLUS((NUM 5), (NUM 6)), (NUM 11))", test_formula, expected)

let test25 =
	let test_formula = LESS(PLUS((NUM 5), (NUM 6)), (NUM 12)) in
	let expected = true in
	test_runner("LESS(PLUS((NUM 5), (NUM 6)), (NUM 12))", test_formula, expected)
(* end of test code *)
*)