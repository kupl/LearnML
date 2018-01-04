(*
Name: Yoon Jae Nam (2012-81338)
Organization: Seoul National University
Class: Programming Language (4190.310)
Problem: 7: natural numbers
*)

(* Provided type declaration *)
type nat = ZERO | SUCC of nat

(* function that adds two nat's and returns a nat *)
let rec natadd : nat * nat -> nat = fun (n_left, n_right) ->
	match (n_left, n_right) with
	(ZERO, _) -> n_right
	| (SUCC(n_left_less), _) ->  SUCC((natadd(n_left_less, n_right)))
	
(* function that multiplies two nat's and returns a nat *)
let rec natmul : nat * nat -> nat = fun (n_left, n_right) ->
	match (n_left, n_right) with
	(ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (SUCC(n_left_less), _) ->
		(natadd(n_right, natmul(n_left_less, n_right)))

(* Below: for testing *)
(*
(* Function that evaluates a nat to an int *)
let rec evalNat : nat -> int = fun nat_foo ->
	match nat_foo with
	ZERO -> 0
	| SUCC (nat_bar) -> 1 + (evalNat nat_bar)

let testRunner (nat_left, nat_right, f, expected) =
	let rec natToString : nat -> string = fun nat_foo ->
		match nat_foo with
		ZERO -> "ZERO"
		| SUCC (nat_bar) -> ("SUCC(" ^ (natToString nat_bar) ^ ")")
	in
	let func_name = if (f == natadd) then "natadd" else "natmul" in
	let test_name = func_name ^ "(" ^ (natToString nat_left) ^ ", " ^ (natToString nat_right) ^ ")" in
	let nat_result = f(nat_left, nat_right) in
	let actual = evalNat nat_result in
		print_endline ("---------------");
		if actual = expected then
			(print_string ("Good (" ^ test_name ^ "): ");
			print_int (actual);
			print_newline ()
			)
		else
			(print_endline ("***BAD (" ^ test_name ^ ")");
			print_string "Expected: ";
			print_int(expected);
			print_newline ();
			print_string "Actual: ";
			print_int(actual);
			print_newline ();
			raise (Invalid_argument "testRunner"))
(* end of testRunner *)

let testRunnerSimple (test_name, nat_foo, expected) =
	let actual = evalNat(nat_foo) in
		print_endline ("---------------");
		if actual = expected then
			(print_string ("Good (" ^ test_name ^ "): ");
			print_int(actual);
			print_newline ()
			)
		else
			(print_endline ("***BAD (" ^ test_name ^ ")");
			print_string "Expected: ";
			print_int(expected);
			print_newline ();
			print_string "Actual: ";
			print_int(actual);
			print_newline ();
			raise (Invalid_argument "test_runner"))

let test1 =
	let nat_left = ZERO in
	let nat_right = SUCC(ZERO) in
	let test_func = natadd in
	let expected = 1 in
	testRunner (nat_left, nat_right, test_func, expected)
	
let test2 =
	let nat_left = SUCC(ZERO) in
	let nat_right = SUCC(ZERO) in
	let test_func = natadd in
	let expected = 2 in
	testRunner (nat_left, nat_right, test_func, expected)
	
let test3 =
	let nat_left = SUCC(SUCC(SUCC(ZERO))) in
	let nat_right = ZERO in
	let test_func = natadd in
	let expected = 3 in
	testRunner (nat_left, nat_right, test_func, expected)
	
let test4 =
	let nat_left = SUCC(SUCC(SUCC(ZERO))) in
	let nat_right = ZERO in
	let test_func = natmul in
	let expected = 0 in
	testRunner (nat_left, nat_right, test_func, expected)
	
let test5 =
	let nat_left = SUCC(SUCC(SUCC(ZERO))) in
	let nat_right = SUCC(SUCC(ZERO)) in
	let test_func = natmul in
	let expected = 6 in
	testRunner (nat_left, nat_right, test_func, expected)
	
let test6 =
	let nat_left = ZERO in
	let nat_right = ZERO in
	let test_func = natmul in
	let expected = 0 in
	testRunner (nat_left, nat_right, test_func, expected)
	
let test7 =
	let expr = natmul(
		natadd(SUCC(SUCC(ZERO)), SUCC(ZERO)), (* 3 *)
		natmul(SUCC(SUCC(SUCC(ZERO))), SUCC(SUCC(SUCC(ZERO)))) (* 9 *)
	) in
	testRunnerSimple("test7", expr, 27)
	
let test8 =
	let expr = natadd(
		natadd(SUCC(SUCC(ZERO)), SUCC(ZERO)), (* 3 *)
		natmul(SUCC(SUCC(SUCC(ZERO))), SUCC(SUCC(SUCC(ZERO)))) (* 9 *)
	) in
	testRunnerSimple("test8", expr, 12)

let test9 =
	let expr = natmul(
		natadd(
			natmul(SUCC(SUCC(ZERO)), SUCC(SUCC(SUCC(SUCC(ZERO))))), (* 8 *)
			natmul(ZERO, SUCC(SUCC(ZERO))) (* 0 *)
		), (* 8 *)
		natmul(SUCC(SUCC(SUCC(ZERO))), SUCC(SUCC(SUCC(ZERO)))) (* 9 *)
	) in
	testRunnerSimple("test9", expr, 72)
(* end of test code *)
*)