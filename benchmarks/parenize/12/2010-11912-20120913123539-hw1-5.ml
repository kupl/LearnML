type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Norway | Sweden | England
| Argentina
type tourna = LEAF of team
| NODE of tourna * tourna

let rec parenize(input)=
	match input with
	|NODE(a,b) -> "("^parenize(a)^" "^parenize(b)^")"
	|LEAF a -> (match a with
				|Korea -> "Korea" 
				|France -> "France"
				|Usa -> "USA" 
				|Brazil -> "Brazil"
				|Japan -> "Japan"
				|Nigeria -> "Nigeria" 
				|Cameroon -> "Cameroon"
				|Poland -> "Cameroon" 
				|Portugal -> "Portugal" 
				|Italy -> "Italy" 
				|Germany -> "Germany" 
				|Norway -> "Norway" 
				|Sweden -> "Sweden" 
				|England -> "England"
				|Argentina -> "Argentina")
