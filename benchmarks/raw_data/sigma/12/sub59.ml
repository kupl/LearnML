(*
Name: Yoon Jae Nam (2012-81338)
Organization: Seoul National University
Class: Programming Language (4190.310)
Problem: 2: Sigma
*)

(* Function that computes the sum of f(i), where i ranges 
from a to b (inclusive) *)
let rec sigma : int * int * (int -> int) -> int = fun (a, b, f) ->
	if (a > b) then 0
	else
		f(a) + sigma(a + 1, b, f)

(* Below: test code *)
(*
(* For testing. Prints out the information about incorrect result *)
let printBadResult(test_name, expected, actual) =
	print_endline "Bad";
	print_endline test_name;
	print_endline "Expected:";
	print_int expected;
	print_newline ();
	print_endline "Actual:";
	print_int actual;
	print_newline ()
		
let test1 =
	let a = 1 in
	let b = 3 in
	let testFunc = (fun n -> n * n) in
	let expected = 14 in
	let actual = sigma(a, b, testFunc) in
	if actual = expected then print_endline "Good"
	else printBadResult("test1", expected, actual)
	
let test2 =
	let a = 3 in
	let b = 1 in
	let testFunc = (fun n -> n * n) in
	let expected = 0 in
	let actual = sigma(a, b, testFunc) in
	if actual = expected then print_endline "Good"
	else printBadResult("test1", expected, actual)
	
let test3 =
	let a = -1 in
	let b = 1 in
	let testFunc = (fun n -> n) in
	let expected = 0 in
	let actual = sigma(a, b, testFunc) in
	if actual = expected then print_endline "Good"
	else printBadResult("test1", expected, actual)
	
let test4 =
	let a = 1 in
	let b = 1 in
	let testFunc = (fun n -> n + 1) in
	let expected = 2 in
	let actual = sigma(a, b, testFunc) in
	if actual = expected then print_endline "Good"
	else printBadResult("test1", expected, actual)
(* end of test code *)
*)