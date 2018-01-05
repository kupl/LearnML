(* School of Computer Science & Engineering
 * 2009-23151
 * Sungkeun Cho
 * HW 2 - Exercise 1
 *)



type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	    | Poland | Portugal | Italy | Germany | Sweden | England
	    | Croatia | Argentina;;
type tourna = LEAF of team
	      | NODE of tourna * tourna;;

let rec toParen t=
  let team2s teamName =
    match teamName with
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
  in
  match t with
      LEAF(teamName) -> (team2s teamName)
    | NODE(t1,t2) -> "("^(toParen t1)^" "^(toParen t2)^")";;

(* 
toParen (LEAF Korea);;
toParen (NODE(LEAF Korea,LEAF Brazil));;
toParen (NODE (NODE(LEAF Korea,LEAF Brazil),LEAF Italy));;
toParen (NODE (NODE(LEAF Korea,LEAF Korea),
	       (NODE(LEAF Italy,LEAF Japan))));;
*)
