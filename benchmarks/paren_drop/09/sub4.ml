(* Computer Science/2005-11759/Sangcheol Park/Exercise 2-2. *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina;;
type tourna = LEAF of team | NODE of tourna * tourna;;

let rec drop(tree, str) =
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
		| Argentina -> "Argentina" in
	let rec string_to_team str =
		match str with
		| "Korea" -> Korea
		| "France" -> France
		| "Usa" -> Usa
		| "Brazil" -> Brazil
		| "Japan" -> Japan
		| "Nigeria" -> Nigeria
		| "Cameroon" -> Cameroon
		| "Poland" -> Poland
		| "Portugal" -> Portugal
		| "Italy" -> Italy
		| "Germany" -> Germany
		| "Sweden" -> Sweden
		| "England" -> England
		| "Croatia" -> Croatia
		| "Argentina" -> Argentina in
	let rec toParen t =
		match t with
		| NODE(a, b) -> String.concat "" ["("; toParen a; " "; toParen b; ")"]
		| LEAF a -> (team_to_string a) in
	let team_to_drop = (string_to_team str) in
	let strange_case = NODE(LEAF team_to_drop, LEAF team_to_drop) in
	let rec drop_helper(a, b) = match a with
		| NODE(LEAF lt, LEAF rt) -> if lt = team_to_drop
				then LEAF rt
				else if rt = team_to_drop
				then LEAF lt
				else NODE(LEAF lt, LEAF rt)
		| NODE(left, LEAF team) -> if team = team_to_drop
				then drop_helper(left, team_to_drop)
				else NODE(drop_helper(left, team_to_drop), LEAF team)
		| NODE(LEAF team, right) -> if team = team_to_drop
				then drop_helper(right, team_to_drop)
				else NODE(LEAF team, drop_helper(right, team_to_drop))
		| NODE(left, right) ->
				if left = strange_case
				then drop_helper(right, team_to_drop)
				else if right = strange_case
				then drop_helper(left, team_to_drop)
				else NODE(drop_helper(left, team_to_drop), drop_helper(right, team_to_drop))
		| LEAF k -> LEAF k in
	if (tree = strange_case) then "" else toParen(drop_helper(tree, team_to_drop))
;;

(* 
drop(NODE(LEAF Korea, LEAF Korea), "Korea");;
drop(NODE(NODE(LEAF Korea, LEAF France), LEAF Brazil), "Brazil");;
drop(NODE(NODE(LEAF Korea, LEAF France), LEAF Brazil), "France");;
drop(NODE(NODE(LEAF Korea, LEAF France), LEAF Brazil), "Korea");;
drop(NODE(NODE(LEAF Korea, LEAF France), NODE(LEAF Korea, LEAF Brazil)), "Korea");;
drop(NODE(NODE(LEAF Korea, LEAF France), NODE(LEAF Korea, LEAF Korea)), "Korea");;
drop(NODE(NODE(LEAF Korea, LEAF France), NODE(LEAF Brazil, NODE(LEAF Usa, LEAF Cameroon))), "Korea");;
*)