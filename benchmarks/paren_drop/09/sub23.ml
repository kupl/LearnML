(* School of Computer Science & Engineering
 * 2009-23151
 * Sungkeun Cho
 * HW 2 - Exercise 2
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

let rec drop(t,teamName) =
  let rec exist(t,teamName)=
    match t with
	LEAF(t1) -> (t1 = teamName)
      | NODE(t1,t2) -> (exist(t1,teamName) || exist(t2,teamName))
  in
  let rec drop2t(t,teamName)=
    match t with
	LEAF(_) -> t
      | NODE(t1,t2) -> 
	  if t1 = LEAF(teamName) 
	  then (drop2t (t2,teamName))
	  else if t2 = LEAF(teamName)
	  then (drop2t (t1,teamName))
	  else
	    NODE(drop2t (t1,teamName),drop2t (t2,teamName))
  in
    if (t=LEAF(teamName))
    then ""
    else if (exist(t,teamName))
    then drop(drop2t(t,teamName),teamName)
    else toParen(t);;


(*
drop((LEAF Korea),Japan);;
drop((LEAF Korea),Korea);;
drop((NODE(LEAF Korea,LEAF Brazil)),Brazil);;
drop((NODE (NODE(LEAF Korea,LEAF Brazil),LEAF Italy)),Brazil);;
drop((NODE (NODE(LEAF Korea,LEAF Korea),
	       (NODE(LEAF Italy,LEAF Japan)))),Korea);;
drop((NODE (NODE(LEAF Korea,LEAF Korea),
	       (NODE(LEAF Korea,LEAF Korea)))),Korea);;
*)
