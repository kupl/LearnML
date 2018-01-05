(* 컴퓨터공학부/2006-11855/정용혁/HW2-ex1 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		  | Poland | Portugal | Italy | Germany | Sweden | England
		  | Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

let rec toParen t = match t with
  NODE (t1, t2) -> "(" ^ (toParen t1) ^ " " ^ (toParen t2) ^ ")"
| LEAF team -> match team with
    Korea -> "Korea" | France -> "France" | Usa -> "Usa" 
  | Brazil -> "Brazil" | Japan -> "Japan" | Nigeria -> "Nigeria" 
  | Cameroon -> "Cameroon" | Poland -> "Poland" | Portugal -> "Portugal"
  | Italy -> "Italy" | Germany -> "Germany" | Sweden -> "Sweden"
  | England -> "England" | Croatia -> "Croatia" | Argentina -> "Argentina"
