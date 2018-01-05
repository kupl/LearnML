(* 2006-11782 Song Young-chan, Hw2-2 drop *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland
          | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team
	    | NODE of tourna * tourna

let rec drop (input_tourna, target) =
	match input_tourna with
	  LEAF(team_a) -> if (team_a = target) then ""
	  		else 
			(	match team_a with
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
			)
	| NODE(LEAF(team_a), LEAF(team_b)) -> if (team_a = target) then (drop (LEAF(team_b), target))
					      else if (team_b = target) then (drop (LEAF(team_a), target))
						   else "("^(drop (LEAF(team_a), target))^" "^(drop (LEAF(team_b), target))^")"
	| NODE(LEAF(team_a), right) -> if (team_a = target) then (drop (right, target))
					     else if ((drop (right, target)) = "") then (drop (LEAF(team_a), target))
						  else "("^(drop (LEAF(team_a), target))^" "^(drop (right, target))^")"
	| NODE(left, LEAF(team_b)) -> if (team_b = target) then (drop (left, target))
					     else if ((drop (left, target)) = "") then (drop (LEAF(team_b), target))
						  else "("^(drop (left, target))^" "^(drop (LEAF(team_b), target))^")"
