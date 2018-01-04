(*2009-11718 1-4*)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon 
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina
type tourna = LEAF of team
	| NODE of tourna * tourna

let rec parenize tour =
	match tour with
	LEAF a -> (match a with
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
		| Argentina -> "Argentina")
	| NODE (a,b) -> "("^(parenize a)^" "^(parenize b)^")"
