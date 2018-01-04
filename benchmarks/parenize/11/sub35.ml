type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina

type tourna =
	| LEAF of team
	| NODE of tourna * tourna

let rec parenize t =
	let string_of_team team = match team with
	| Korea	-> "Korea"
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
	| Sweden -> "Sweden"
	| England -> "England"
	| Croatia -> "Croatia"
	| Argentina -> "Argentina"
	in
	match t with
	| LEAF l -> string_of_team l
	| NODE (t1, t2) -> "("^parenize t1^" "^parenize t2^")"

(*
let _ = print_endline (parenize (NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)))
*)
