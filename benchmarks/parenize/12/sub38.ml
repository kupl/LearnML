type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Norway | Sweden | England | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let rec parenize tour =
	let toString tour =
		match tour with
		Korea -> "Korea" | France -> "France" | Usa -> "Usa" | Brazil -> "Brazil" | Japan -> "Japan"
		| Nigeria -> "Nigeria" | Cameroon -> "Cameroon" | Poland -> "Poland" | Portugal -> "Portugal"
		| Italy -> "Italy" | Germany -> "Germany" | Norway -> "Norway" | Sweden -> "Sweden" | England -> "England"
		| Argentina -> "Argentina"
	in
	match tour with
	LEAF(t) -> toString t
	| NODE(t1, t2) -> String.concat " " [String.concat "" ["("; parenize t1]; String.concat "" [parenize t2; ")"]]
