type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Norway | Sweden | England
| Argentina

type tourna = LEAF of team
| NODE of tourna * tourna

let rec parenize t =
	match t with
	| NODE(n1,n2) -> "("^(parenize n1)^" "^(parenize n2)^")"
	| LEAF Korea -> "Korea"
	| LEAF France-> "France"
	| LEAF Usa -> "Usa"
	| LEAF Brazil -> "Brazil"
	| LEAF Japan -> "Japan"
	| LEAF Nigeria -> "Nigeria"
	| LEAF Cameroon -> "Cameroon"
	| LEAF Poland -> "Poland"
	| LEAF Portugal -> "Portugal"
	| LEAF Italy -> "Italy"
	| LEAF Germany -> "Germany"
	| LEAF Sweden -> "Sweden"
	| LEAF Norway -> "Norway"
	| LEAF England -> "England"
	| LEAF Argentina -> "Argentina"
;;