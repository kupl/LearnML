type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team
			| NODE of tourna * tourna

exception Error of string

let getTeam t = 
	match t with
	Korea -> "Korea"
	|France -> "France"
	|Usa -> "Usa"
	|Brazil -> "Brazil"
	|Japan -> "Japan"
	|Nigeria -> "Nigeria"
	|Cameroon -> "Cameroon"
	|Poland -> "Poland"
	|Portugal -> "Portugal"
	|Italy -> "Italy"
	|Germany -> "Germany"
	|Sweden -> "Sweden"
	|England -> "England"
	|Croatia -> "Croatia"
	|Argentina -> "Argentina"


let rec parenize tour = 
	match tour with
	LEAF t -> getTeam t
	| NODE (t1, t2) -> "("^(parenize t1)^" "^(parenize t2)^")"


