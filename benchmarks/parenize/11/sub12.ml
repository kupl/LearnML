(* HW 1-4 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina

type tourna = LEAF of team
	| NODE of tourna * tourna

let string_of_team t = 
	match t with
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
	| Sweden -> "Sweden"
	| England -> "England"
	| Croatia -> "Croatia"
	| Argentina -> "Argentina"

let rec parenize t = 
	match t with
	LEAF x -> (string_of_team x)
	| NODE (x, y) -> "(" ^ (parenize x) ^ " " ^ (parenize y) ^ ")"