(*
Name: Yoon Jae Nam (2012-81338)
Organization: Seoul National University
Class: Programming Language (4190.310)
Problem: 9: zipper and tree
*)

exception NOMOVE of string 

(* Provided type declarations *)
type item = string
type tree = LEAF of item
			| NODE of tree list
type zipper = TOP
			| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
(* end of provided type declarations *)

(* go to the sibling immediately at the right *)
let goRight : location -> location = fun loc ->
	match loc with
	LOC(t,TOP) -> raise (NOMOVE "right of top")
	| LOC(t,HAND(left,up,r::right)) -> 
		LOC(r,HAND(t::left,up,right))
	| LOC(t,HAND(left,up,[])) -> raise (NOMOVE "right of last")
(* end of goRight *)

(* Combines a tree with its left and right siblings.
for example, the sibling are 1 2 3 4 5 (from left to right) 
in the comments written in the function below:
left: [2;1], t: [3], right: [4;5] *)
let combineChildren : tree list * tree * tree list -> tree list = 
	fun (left,t,right) ->
		let left_and_t = t::left in (* [3;2;1] *)
		let left_and_t_reversed = List.rev left_and_t in (* [1;2;3] *)
		List.append left_and_t_reversed right (* [1;2;3;4;5] *)
(* end of combineChildren *)

(* Go to the parent node *)
let goUp : location -> location = fun loc ->
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left,up,right)) ->
		let siblings_united : tree list = 
			combineChildren(left, t, right) in
			LOC(NODE(siblings_united), up)	
(* end of goUp *)

(* Go to the leftmost child. If impossible, throw NOMOVE *)
let goDown : location -> location = fun loc ->
	match loc with
	LOC(LEAF(name),current_zipper) -> (* cannot go down from a leaf *)
		raise (NOMOVE "down of leaf")
	| LOC(NODE(tree_list),current_zipper) ->
		match tree_list with
		[] -> raise (NOMOVE "no children")
		| h::t -> (* h : tree, t : tree list *)
			LOC(
				h, (* tree of the leftmost child *)
				HAND(
					[], (* no left siblings of the leftmost child *)
					current_zipper, 
						(* current zipper becomes the parent zipper *)
					t (* right siblings of the leftmost child *)
				)
			)
(* end of goDown *)

(* Below: test code *)
(*
(* goLeft (provided) *)
let goLeft : location -> location = fun loc -> 
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")
(* end of goLeft *)

(* tree to a string list (left to right) *)
let rec treeToStringList : tree * string list -> string list = fun (tree_foo,prev_list) ->
	match tree_foo with
	LEAF(name) -> List.rev (name::(List.rev prev_list))
	| NODE(tree_list) ->
		let addEmptyList = fun t -> (t,[]) in
		let tree_list_with_empty_list =
			List.map addEmptyList tree_list in
		let tree_list_string_list = List.map treeToStringList tree_list_with_empty_list in
		let tree_list_string_list_combined = List.concat tree_list_string_list in
		(List.append prev_list tree_list_string_list_combined)
(* end of treeToStringList *)

(* a * b + c * d *)
let testTree = NODE [ 
	NODE [LEAF "a"; LEAF "*"; LEAF "b"];
	LEAF "+";
	NODE [LEAF "c"; LEAF "*"; LEAF "d"]
]
	
let printStringList = fun string_list ->
	List.iter (fun str -> print_string (str ^ " ")) string_list

let testRunner = fun (data, header, loc, f, f_name, no_error, expected) ->
	print_endline "--------------------------------------------------";
	Printf.printf "%s\nData: %s\n=====\nloc:\n" header data;
	let curr_loc_string_list =
		match loc with 
		LOC(t, _) -> treeToStringList(t, []) 
	in
	printStringList(curr_loc_string_list);
	print_newline ();
	Printf.printf "=====\n%s(loc):\n" f_name;
	if no_error then (* no error should be raised *)
		let loc_result = (f loc) in
			match loc_result with
			LOC(t, _) ->
				let actual = treeToStringList(t, []) in
					if actual = expected then
						(printStringList(expected); print_newline ();
						print_endline "                                          Good!";
						print_newline ())
					else
						(print_endline "                                          BAD!! Expected and actual are different";
						print_endline "Expected:";
						printStringList(expected);
						print_newline ();
						print_endline "Actual:";
						printStringList(actual);
						print_newline ();
						raise (NOMOVE "WRONG RESULT"))
	else
		let _ = 
			try 
				let _ = (f loc) in
					(print_endline "                                          BAD!! ERROR WAS EXPECTED";
					raise (NOMOVE "WRONG RESULT")
					)
			with 
				| NOMOVE msg ->
					(print_endline "                                          GOOD! Error is caught :D";
					Printf.printf "Error message: %s\n" msg
					)
				| _ -> () 
		in ()
(* end of testRunner *)
		
(* Test 1: testing with (a * b + c * d) *)
(* location of second mul *)
let locSecondMul = LOC (
	LEAF "*", (* current tree *)
	HAND( (* zipper *)
		[LEAF "c"], (* left siblings : tree list *)
		HAND( (* zipper of the parent node *)
			[LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], (* left siblings : tree list *)
			TOP, (* zipper of parent *)
			[] (* right siblings *)
		),
		[LEAF "d"] (* right siblings : tree list *)
	)
)

(* location of the second node (c * d) *)
let locSecondBigNode = LOC (
	NODE [LEAF "c"; LEAF "*"; LEAF "d"], (* current tree *)
	HAND( (* zipper *)
		[LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], (* left siblings : tree list *)
		TOP, (* zipper of parent *)
		[] (* right siblings : tree list *)
	)
)

(* location of the second node (c * d) *)
let locPlus = LOC (
	LEAF "+", (* current tree *)
	HAND( (* zipper *)
		[NODE [LEAF "a"; LEAF "*"; LEAF "b"]], (* left siblings : tree list *)
		TOP, (* zipper of parent *)
		[NODE [LEAF "c"; LEAF "*"; LEAF "d"]] (* right siblings : tree list *)
	)
)

(* location of the top *)
let locTop = LOC (
	testTree, (* current tree *)
	TOP
)

(* End of location definitions *)

(* test1: second "*" *)
(* left of second "*" *)
let test_second_mul_left =
	let data = "a * b + c * d" in
	let header = "second_mul_left. Printing left of second mul" in
	let loc = locSecondMul in
	let f = goLeft in
	let f_name = "goLeft" in
	testRunner(data, header, loc, f, f_name, true, ["c"])

(* right of second "*" *)
let test_second_mul_right =
	let data = "a * b + c * d" in
	let header = "second_mul_right. Printing right of second mul" in
	let loc = locSecondMul in
	let f = goRight in
	let f_name = "goRight" in
	testRunner(data, header, loc, f, f_name, true, ["d"])

(* up of second "*" *)
let test_second_mul_up =
	let data = "a * b + c * d" in
	let header = "second_mul_up. Printing up of second mul" in
	let loc = locSecondMul in
	let f = goUp in
	let f_name = "goUp" in
	testRunner(data, header, loc, f, f_name, true, ["c";"*";"d"])

(* down of second "*" *)
let test_second_mul_down =
	let data = "a * b + c * d" in
	let header = "second_mul_down. Printing down of second mul" in
	let loc = locSecondMul in
	let f = goDown in
	let f_name = "goDown" in
	testRunner(data, header, loc, f, f_name, false, [])

(* end of test1: second * *)

(* test2: (c * d) *)
(* left of second node (c * d) *)
let test_second_node_left =
	let data = "a * b + c * d" in
	let header = "second_node_left. Printing left of second node" in
	let loc = locSecondBigNode in
	let f = goLeft in
	let f_name = "goLeft" in
	testRunner(data, header, loc, f, f_name, true, ["+"])

(* 
ERROR EXPECTED
right of second node (c * d) 
*)
let test_second_node_right =
	let data = "a * b + c * d" in
	let header = "(ERROR EXPECTED) second_node_right. Printing right of second node" in
	let loc = locSecondBigNode in
	let f = goRight in
	let f_name = "goRight" in
	testRunner(data, header, loc, f, f_name, false, []) (* change to true test error *)

(* up of second node (c * d) *)
let test_second_node_up =
	let data = "a * b + c * d" in
	let header = "second_node_up. Printing up of second node" in
	let loc = locSecondBigNode in
	let f = goUp in
	let f_name = "goUp" in
	testRunner(data, header, loc, f, f_name, true, ["a";"*";"b";"+";"c";"*";"d"])

(* down of second node (c * d) *)
let test_second_node_down =
	let data = "a * b + c * d" in
	let header = "second_node_down. Printing up of second node" in
	let loc = locSecondBigNode in
	let f = goDown in
	let f_name = "goDown" in
	testRunner(data, header, loc, f, f_name, true, ["c"])
(* end of test2: (c * d) *)

(* test3: + *)
(* left of plus *)
let test_plus_left =
	let data = "a * b + c * d" in
	let header = "plus_left. Printing left of plus" in
	let loc = locPlus in
	let f = goLeft in
	let f_name = "goLeft" in
	testRunner(data, header, loc, f, f_name, true,["a";"*";"b"])

(* right of plus *)
let test_plus_right =
	let data = "a * b + c * d" in
	let header = "plus_right. Printing right of plus" in
	let loc = locPlus in
	let f = goRight in
	let f_name = "goRight" in
	testRunner(data, header, loc, f, f_name, true,["c";"*";"d"])

(* up of plus *)
let test_plus_up =
	let data = "a * b + c * d" in
	let header = "plus_up. Printing up of plus" in
	let loc = locPlus in
	let f = goUp in
	let f_name = "goUp" in
	testRunner(data, header, loc, f, f_name, true, ["a";"*";"b";"+";"c";"*";"d"])

(* down of plus *)
let test_plus_down =
	let data = "a * b + c * d" in
	let header = "Printing down of plus" in
	let loc = locPlus in
	let f = goDown in
	let f_name = "goDown" in
	testRunner(data, header, loc, f, f_name, false, [])
(* end of test3: + *)

(* test4: top *)
(* left of top *)
let test_top_left =
	let data = "a * b + c * d" in
	let header = "Printing left of top" in
	let loc = locTop in
	let f = goLeft in
	let f_name = "goLeft" in
	testRunner(data, header, loc, f, f_name, false, [])

(* right of top *)
let test_top_right =
	let data = "a * b + c * d" in
	let header = "Printing right of top" in
	let loc = locTop in
	let f = goRight in
	let f_name = "goRight" in
	testRunner(data, header, loc, f, f_name, false, [])

(* up of top *)
let test_top_up =
	let data = "a * b + c * d" in
	let header = "Printing up of top" in
	let loc = locTop in
	let f = goUp in
	let f_name = "goUp" in
	testRunner(data, header, loc, f, f_name, false, [])

(* down of top *)
let test_top_down =
	let data = "a * b + c * d" in
	let header = "Printing down of top" in
	let loc = locTop in
	let f = goDown in
	let f_name = "goDown" in
	testRunner(data, header, loc, f, f_name, true, ["a";"*";"b"])

(* end of test4: top *)

(* end of test code *)
*)