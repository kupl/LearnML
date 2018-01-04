type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna


let rec parenize _tourna =
	let teamToString _team =
		match _team with
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
	in

	match _tourna with
	NODE(_tourna1, _tourna2) -> "(" ^ (parenize _tourna1) ^ " " ^ (parenize _tourna2) ^ ")"
	| LEAF _teamname -> (teamToString _teamname)
