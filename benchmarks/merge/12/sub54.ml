(*
Name: Yoon Jae Nam (2012-81338)
Organization: Seoul National University
Class: Programming Language (4190.310)
Problem: 1: List sum
*)

(* Function that merges two lists in descending order into one list, 
which is also in descending order *)
let rec merge : int list * int list -> int list = fun (l1, l2) ->
	match (l1, l2) with
	([], _) -> l2
	| (_, []) -> l1
	| (h1::t1, h2::t2) ->
		if (h1 <= h2) then h2::merge(l1, t2)
		else h1::merge(t1, l2)
(* end of merge *)

(* Below: test code *)
(*
(* For testing. Prints out an int list *)
let printIntlist l =
	List.iter (fun num -> print_char ' '; print_int num) l

(* For testing. Returns true if l1 and l2 are equal. False otherwise *)
let rec twoListsEqual(l1, l2) =
	if ((List.length l1) != (List.length l2)) then false
	else
		 match (l1, l2) with
		([], []) -> true
		| ([], _) -> false
		| (_, []) -> false
		| (h1::t1, h2::t2) ->
			if (h1 != h2) then false
			else twoListsEqual(t1, t2)
	
(* 
For testing. Data for testing. In each test case,
Arg 1: l1
Arg 2: l2
Arg 3: expected l12 (l1 and l2 merged) 
*)
let testData = [
	[[5;4;3]; [5;4;3]; [5;5;4;4;3;3]];
	[[5;4;3]; [2;1]; [5;4;3;2;1]];
	[[]; []; []];
	[[100; 10; 3; -1]; [13; 7; -5]; [100; 13; 10; 7; 3; -1; -5]];
	[[5;5]; [5;5;5]; [5;5;5;5;5]];
	[[]; [5]; [5]];
	[[5]; []; [5]];
	[[5; 4; 3]; [7; 4; 3; -1]; [7; 5; 4; 4; 3; 3; -1]];
]

(* For testing. Checks a single test case, where the test case comprises of three lists *)
let checkTestCase = fun data ->
	let l1 = List.nth data 0 in
	let l2 = List.nth data 1 in
	let l12_expected = List.nth data 2 in
	let l12_actual = merge(l1, l2) in
		if (twoListsEqual(l12_actual, l12_expected)) then print_endline "Good"
		else (print_endline "Bad for the following test case";
			print_endline "List 1:";
			printIntlist l1;
			print_newline();
			print_endline "List 2:";
			printIntlist l2;
			print_newline();
			print_endline "Expected merged list:";
			printIntlist l12_expected;
			print_newline();
			print_endline "Actual merged list:";
			printIntlist l12_actual;
			print_newline())
	
(* For testing. Runs the test *)
let testRunner =	
	let test_data = testData in
		List.iter checkTestCase test_data
(* end of test code *)
*)