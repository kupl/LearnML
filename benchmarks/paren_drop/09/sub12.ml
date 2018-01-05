(* 컴퓨터공학부/2006-11855/정용혁/HW2-ex2 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		  | Poland | Portugal | Italy | Germany | Sweden | England
		  | Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

let rec toParen tournament = match tournament with
  NODE (t1, t2) -> "(" ^ (toParen t1) ^ " " ^ (toParen t2) ^ ")"
| LEAF team -> match team with
    Korea -> "Korea" | France -> "France" | Usa -> "Usa" 
  | Brazil -> "Brazil" | Japan -> "Japan" | Nigeria -> "Nigeria" 
  | Cameroon -> "Cameroon" | Poland -> "Poland" | Portugal -> "Portugal"
  | Italy -> "Italy" | Germany -> "Germany" | Sweden -> "Sweden"
  | England -> "England" | Croatia -> "Croatia" | Argentina -> "Argentina"

let drop (tour, team) =
  let rec isDrop (tour, team) = match tour with
    LEAF t -> if t = team then true else false
  | NODE (t1, t2) -> isDrop (t1, team) && isDrop (t2, team)
  in
  let rec toDrop (tour, team) = match tour with
    LEAF t -> LEAF t
  | NODE (t1, t2) -> 
      if isDrop (t1, team) then toDrop (t2, team)
	  else if isDrop (t2, team) then toDrop (t1, team)
	  else NODE ((toDrop (t1, team)), (toDrop (t2, team)))
  in
    if isDrop (tour, team) then ""
	else toParen (toDrop (tour, team))
