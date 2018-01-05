type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna

let rec toParen t1 = let toStr t1 = match t1 with
  			Korea -> "Korea"
  			|France -> "France"
  			|Usa -> "Usa"
  			|Brazil -> "Brazil"
  			|Japan -> "Japan"
  			|Nigeria -> "Nigeria"
  			|Cameroon -> "Cameroon"
  			|Poland -> "Poland"
  			|Italy -> "Italy"
  			|Portugal -> "Portugal"
  			|Germany -> "Germany"
  			|Sweden -> "Sweden"
  			|England -> "England"
  			|Croatia -> "Croatia"
  			|Argentina -> "Argentina" in
			
			match t1 with
  			(NODE (a,b)) ->"("^(toParen a)^" "^(toParen b)^")"  			
			|(LEAF a) -> (toStr a);;