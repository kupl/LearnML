(* 2008-11874 Lee, Sujee *)
(* EXERCISE 4 *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina 
type tourna = LEAF of team
| NODE of tourna * tourna

let rec parenize tour = (* parenize : tourna -> string = <fun> *)
	match tour with
		| LEAF(Korea) -> "Korea" (* to deal with the type team as a string *)
		| LEAF(France) -> "France"
		| LEAF(Usa) -> "Usa"
		| LEAF(Brazil) -> "Brazil"
		| LEAF(Japan) -> "Japan"
		| LEAF(Nigeria) -> "Nigeria"
		| LEAF(Cameroon) -> "Cameroon"
		| LEAF(Poland) -> "Poland"
		| LEAF(Portugal) -> "Portugal"
		| LEAF(Italy) -> "Italy"
		| LEAF(Germany) -> "Germany"
		| LEAF(Sweden) -> "Sweden"
		| LEAF(England) -> "England"
		| LEAF(Croatia) -> "Croatia"
		| LEAF(Argentina) -> "Argentina"
		| NODE(LEAF l1 , LEAF l2) -> "("^(parenize(LEAF l1))^" "^parenize(LEAF l2)^")" (* base case *)
		| NODE(LEAF l1 , n1) -> "("^(parenize(LEAF l1))^" "^(parenize n1)^")"
		| NODE(n1 , LEAF l1) -> "("^(parenize n1)^" "^(parenize(LEAF l1))^")"
		| NODE(n1 , n2) -> "("^(parenize n1)^" "^(parenize n2)^")"

(*
let result = parenize(NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil))
let _ =
	print_string "EXERCISE 4 : ";
	print_string result;
	print_newline()
	*)