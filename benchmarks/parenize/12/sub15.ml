(* 2008-11874 EXERCISE 5 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Norway | Sweden | England
| Argentina
type tourna = LEAF of team
| NODE of tourna * tourna

let rec parenize tour = 
	match tour with
		| LEAF(Korea) -> "Korea"
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
		| LEAF(Norway) -> "Norway"
		| LEAF(Sweden) -> "Sweden"
		| LEAF(England) -> "England"
		| LEAF(Argentina) -> "Argentina"
		| NODE(LEAF l1 , LEAF l2) -> "("^(parenize(LEAF l1))^" "^parenize(LEAF l2)^")"
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