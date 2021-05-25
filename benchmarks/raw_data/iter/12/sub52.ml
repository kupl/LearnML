(*
Name: Yoon Jae Nam (2012-81338)
Organization: Seoul National University
Class: Programming Language (4190.310)
Problem: 3: iter
*)

let rec iter : int * ('a -> 'a) -> 'a -> 'a = fun (n, f) ->
	if n = 0 then (fun param -> param)
	else (fun param -> (iter(n - 1, f) (f param)))
(* end of iter *)

(* Below: test code *)
(*
(* For testing. *)
let printResultInt(result, test_name, expected, actual) =
	print_endline "----------------";
	print_endline (result ^ " (" ^ test_name ^ ")");
	print_string "(Expected, Actual): (";
	print_int expected;
	print_string ", ";
	print_int actual;
	print_endline ")"

(* For testing. *)
let printResultString(result, test_name, expected, actual) =
	print_endline "----------------";
	print_endline (result ^ " (" ^ test_name ^ ")");
	print_string "(Expected, Actual): (";
	print_string expected;
	print_string ", ";
	print_string actual;
	print_endline ")"

let test1 =
	let n = 3 in
	let testFunc = (fun x -> 2 + x) in
	let param = 0 in
	let expected = 6 in
	let actual = iter(n, testFunc) param in
	if actual = expected then 
		printResultInt("Good", "test1", expected, actual)
	else printResultInt("Bad", "test1", expected, actual)
	
let test2 =
	let n = 3 in
	let testFunc = (fun str -> (str ^ "- ")) in
	let param = "hello" in
	let expected = "hello- - - " in
	let actual = iter(n, testFunc) param in
	if actual = expected then 
		printResultString("Good", "test2", expected, actual)
	else printResultString("Bad", "test2", expected, actual)
	
let test3 =
	let n = 4 in
	let testFunc = (fun l -> (1::l)) in
	let param = [] in
	let expected = [1;1;1;1] in
	let actual = iter(n, testFunc) param in
	if actual = expected then 
		printResultString("Good", "test3", "abridged", "abridged")
	else printResultString("Bad", "test3", "abridged", "abridged")

let test4 =
	let n = 3 in
	let testFunc = (fun x -> (x * 2)) in
	let param = 3 in
	let expected = 24 in
	let actual = iter(n, testFunc) param in
	if actual = expected then 
		printResultInt("Good", "test4", expected, actual)
	else printResultInt("Bad", "test4", expected, actual)

let test5 =
	let n = 0 in
	let testFunc = (fun x -> (x * 2)) in
	let param = 155 in
	let expected = 155 in
	let actual = iter(n, testFunc) param in
	if actual = expected then 
		printResultInt("Good", "test5", expected, actual)
	else printResultInt("Bad", "test5", expected, actual)

(* end of test code *)
*)