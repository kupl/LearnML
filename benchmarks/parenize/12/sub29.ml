type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Norway | Sweden | England
| Argentina
type tourna = LEAF of team
			  | NODE of tourna * tourna

let make_string t = 
	match t with
	| Korea -> "Korea"
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

let rec parenize tou =
	match tou with 
	| LEAF x -> (make_string x)
	| NODE (x, y) -> "(" ^ (parenize x) ^ " " ^ (parenize y) ^ ")"
	
