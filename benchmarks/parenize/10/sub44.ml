type team = 
	Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina

type tourna = LEAF of team
			| NODE of tourna * tourna

let parenize p_ =
	let rec par p = 
		match p with
		LEAF t -> 
			(match t with
			Korea -> "Korea"
			| France -> "France"
			| Usa -> "Usa"
			| Brazil -> "Brazil"
			| Japan -> "Japan"
			| Nigeria -> "Nigeria"
			| Cameroon -> "Cameroon"
			| Poland -> "Poland"
			| Portugal -> "Protugal"
			| Italy -> "Italy"
			| Germany -> "Germany"
			| Sweden -> "Sweden"
			| England -> "England"
			| Croatia -> "Croatia"
			| Argentina -> "Argentina")
		| NODE (p1, p2) -> "("^(par p1)^" "^(par p2)^")"
	in
	par p_
