type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Norway | Sweden | England
	| Argentina 

type tourna =
	| LEAF of team
	| NODE of tourna * tourna

let string2team team = match team with
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
	| Norway -> "Norway"
	| Sweden -> "Sweden"
	| England -> "England"
	| Argentina -> "Argentina"


let rec parenize tree =
	match tree with
	| LEAF leaf -> string2team leaf
	| NODE ( subtree1, subtree2 ) -> "("^parenize subtree1^" "^parenize subtree2^")"