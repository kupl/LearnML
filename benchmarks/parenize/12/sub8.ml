type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Argentina
	  | Poland | Portugal | Italy | Germany | Norway | Sweden | England 
type tourna = LEAF of team
	    | NODE of tourna * tourna
let chgstr x =
	match x with
	| Korea -> "Korea"
	| France -> "France"
	| Usa -> "Usa"
	| Brazil -> "Brazil"
	| Japan -> "Japan"
	| Nigeria -> "Nigeria"
	| Cameroon -> "Cameroon"
	| Argentina -> "Argentina"
	| Poland -> "Poland"
	| Portugal -> "Portugal"
	| Italy -> "Italy"
	| Germany -> "Germany"
	| Norway -> "Norway"
	| Sweden -> "Sweden"
	| England -> "England"

let rec parenize tna =
	match tna with
	| (LEAF x) -> chgstr x
	| (NODE (a, b)) -> "(" ^ (parenize a) ^ " " ^ (parenize b) ^ ")"

