(* 2009-13384, CHO Hyunik *)



type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
and tourna = LEAF of team | NODE of tourna * tourna

let teamString(team) =
	match team with
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
	| Sweden -> "Sweden"
	| England -> "England"
	| Croatia -> "Croatia"
	| Argentina -> "Argentina"


let rec parenize(tor) =
	match tor with
	LEAF a -> teamString(a)
	| NODE(a, b) -> "("^parenize(a)^" "^parenize(b)^")"