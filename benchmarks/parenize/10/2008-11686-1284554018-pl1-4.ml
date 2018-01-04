type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina
type tourna = LEAF of team
	| NODE of tourna * tourna

let rec parenize : tourna -> string =
	fun t -> match t with 
		NODE (t1,t2) -> String.concat "" ["(";parenize t1;" ";parenize t2;")"]
		|LEAF nara -> (match nara with 
					Korea -> "Korea"
					|France -> "France"
					|Usa -> "USA"
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
					|Argentina -> "Argentina" )
