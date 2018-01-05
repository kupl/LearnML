(*
Name: Yoon Jae Nam (2012-81338)
Organization: Seoul National University
Class: Programming Language (4190.310)
Problem: 5: tournament string
*)

(* Provided type declarations *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria 
			| Cameroon | Poland | Portugal | Italy | Germany | Norway 
			| Sweden | England | Argentina

type tourna = LEAF of team
			| NODE of tourna * tourna
(* End of provided declarations *)

(* teamToString: converts a value of team type into a string *)
let teamToString : team -> string = fun team_foo ->
	match team_foo with
	Korea -> "Korea" 
	| France -> "France"
	| Usa -> "Usa"
	| Brazil -> "Brazil"
	| Japan -> "Japan"
	| Nigeria -> "Nigeria"
	| Cameroon -> "Cameroon"
	| Poland -> "Poland"
	| Portugal -> "Portugal"
	| Italy -> "Italy"
	| Germany -> "Germany"
	| Norway -> "Norway"
	| Sweden -> "Sweden"
	| England -> "England"
	| Argentina -> "Argentina"
(* end of teamToString *)
	
(* parenize function that converts a tourna input into a 1D string *)
let rec parenize : tourna -> string = fun tourna_foo ->
	match tourna_foo with
	LEAF leaf -> teamToString(leaf)
	| NODE(left_tourna, right_tourna) ->
		(let left_string = parenize(left_tourna) in
		let right_string = parenize(right_tourna) in
		"(" ^ left_string ^ " " ^ right_string ^ ")")
(* End of parenize *)

(* Below: for testing *)
(*
let test_runner (test_name, node, expected) =
	let actual = parenize(node) in
		print_endline ("---------------");
		if actual = expected then
			print_endline ("Good (" ^ test_name ^ ")")
		else
			print_endline ("***BAD (" ^ test_name ^ ")");
			print_string "Expected: ";
			print_string expected;
			print_newline ();
			print_string "Actual: ";
			print_string actual;
			print_newline ()

let test1 =
	let node = NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil) in
	let expected = "((Korea Portugal) Brazil)" in
	test_runner("test1", node, expected)
	
let test2 =
	let node = NODE(LEAF Nigeria, LEAF Italy) in
	let expected = "(Nigeria Italy)" in
	test_runner("test2", node, expected)

let test3 =
	let node = LEAF Japan in
	let expected = "Japan" in
	test_runner("test3", node, expected)

let test4 =
	let node = NODE(LEAF Norway, NODE(LEAF France, LEAF Argentina)) in
	let expected = "(Norway (France Argentina))" in
	test_runner("test4", node, expected)

let test5 =
	let node = NODE(NODE(NODE(LEAF Japan, LEAF Nigeria), NODE(LEAF Poland, LEAF Portugal)), NODE(LEAF Norway, LEAF Sweden)) in
	let expected = "(((Japan Nigeria) (Poland Portugal)) (Norway Sweden))" in
	test_runner("test5", node, expected)
(* end of test code *)
*)