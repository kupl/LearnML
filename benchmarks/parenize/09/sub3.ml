(* Computer Science/2005-11759/Sangcheol Park/Exercise 2-1.*)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina;;
type tourna = LEAF of team | NODE of tourna * tourna;;

let rec team_to_string(t) =
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
	| Sweden -> "Sweden"
	| England -> "England"
	| Croatia -> "Croatia"
	| Argentina -> "Argentina"
;;

let rec toParen t =
    match t with
    | NODE(a, b) -> String.concat "" ["(";toParen a; " "; toParen b; ")"]
    | LEAF a -> (team_to_string a)
;;
