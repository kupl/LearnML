(* 5 parenize : tourna -> string *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Norway | Sweden | England | Argentina
type tourna = LEAF of team
	    | NODE of tourna * tourna

let tostring t = match t with
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
	| Norway -> "Norway"
	| Sweden -> "Sweden"
	| England -> "England"
	| Argentina -> "Argentina"

let rec parenize t = match t with
	LEAF l -> tostring l
	| NODE (n0, n1) -> "(" ^ parenize n0 ^ " " ^ parenize n1 ^ ")"

