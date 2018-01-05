(* C:\Users\owner\Desktop\Homework 1(4).ml *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina ;;

type tourna = LEAF of team
| NODE of tourna * tourna ;;

(* Syntax Error or Unbound Value
let rec parenize tour =
	match tour with
	LEAF(Korea) -> "Korea" 
	| LEAF(France) -> "France"
	| LEAF(Usa) -> "Usa"
	| LEAF(Brazil) -> "Brazil"
	| LEAF(Japan) -> "Japan"
	| LEAF(Nigeria) -> "Nigeria"
	| LEAF(Cameroon) -> "Cameroon"
	| LEAF(Poland) -> "Poland"
	| LEAF(Portugal) -> "Portugal"
	| LEAF(Italy) -> "Italy"
	| LEAF(Germany) -> "Germany"
	| LEAF(Sweden) -> "Sweden"
	| LEAF(England) -> "England"
	| LEAF(Croatia) -> "Croatia"
	| LEAF(Argentina) -> "Argentina"
	| NODE(team1, team2) -> "(" ^ perenize(team1) ^ " " ^ perenize(team2) ^ ")" ;;

 *)

let rec parenize tour =
	match tour with
	LEAF(Korea) -> "Korea" 
	| LEAF(France) -> "France"
	| LEAF(Usa) -> "Usa"
	| LEAF(Brazil) -> "Brazil"
	| LEAF(Japan) -> "Japan"
	| LEAF(Nigeria) -> "Nigeria"
	| LEAF(Cameroon) -> "Cameroon"
	| LEAF(Poland) -> "Poland"
	| LEAF(Portugal) -> "Portugal"
	| LEAF(Italy) -> "Italy"
	| LEAF(Germany) -> "Germany"
	| LEAF(Sweden) -> "Sweden"
	| LEAF(England) -> "England"
	| LEAF(Croatia) -> "Croatia"
	| LEAF(Argentina) -> "Argentina"
	| NODE(team1, team2) -> "(" ^ parenize(team1) ^ " " ^ parenize(team2) ^ ")" ;;


