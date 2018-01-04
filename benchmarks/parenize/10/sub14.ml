type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team | NODE of tourna * tourna ;;

let team_string t =
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

let rec parenize tt =
	match tt with
		   LEAF l -> team_string l
		| NODE (left, right) -> "(" ^ (parenize left) ^ " " ^ (parenize right) ^ ")"
;;