(* 
 CSE 2012-11226 Kwak Jin Han
 exercise 1
*)

(*
let a = [7;1]
let b = [9;8;6;5;0]
*)

let rec merge (l1, l2) (* : int list * int list -> int list *) =
	match (l1, l2) with
	| ([], _) -> l2
	| (_, []) -> l1
	| (h1::t1, h2::t2) ->
		if h1>h2 then h1::merge (t1, l2)
		else h2::merge (l1, t2)

(*
let result = merge (a, b)

let rec print_list l =
	match l with
	| [] -> ()
	| (h::t) -> print_int h ; print_string " " ; print_list t

let _ = print_list result
*)

(*
 let _ =
 let rec string_of_list = function
 [] -> ""
 | e::l -> string_of_int e ^ " " ^ string_of_list l
 in
 let assert_equal (expected: int list) (actual: int list) =
 if expected = actual then print_endline "true"
 else
 let expected_string = string_of_list expected in
 let actual_string = string_of_list actual in
 Printf.printf "Expected %s but actual %s\n" expected_string actual_string
 in
 let test_merge (xs: int list) (ys: int list) (expected: int list) =
 merge (xs, ys) |> assert_equal expected
 in
 test_merge [3;2;1] [4;3;2] [4;3;3;2;2;1];
 test_merge [2;1] [10;9] [10;9;2;1];
 test_merge [10;9] [4;3;2] [10;9;4;3;2];
 test_merge [5;3;1] [6;4;2] [6;5;4;3;2;1]
 *)
