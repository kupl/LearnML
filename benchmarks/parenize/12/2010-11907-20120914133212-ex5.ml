type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Norway | Sweden | England | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let rec parenize t = 

let s_of_t t = 
match t with
|Korea -> "Korea"
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
|Norway -> "Norway"
|Sweden -> "Sweden"
|England -> "England"
|Argentina -> "Argentina"

in

match t with
|NODE (a, b) -> String.concat "" ["(";parenize a;" ";parenize b;")"]
|LEAF a -> (s_of_t a)

