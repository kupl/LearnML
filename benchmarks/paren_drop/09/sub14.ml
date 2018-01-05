type team = Korea | France | Usa | Brazil | Japan | Nigeria
| Cameroon | Poland | Portugal | Italy | Germany | Sweden
| England | Croatia | Argentina

type tourna = LEAF of team | NODE of tourna * tourna

let rec drop (tourna, team) =
	let rec toParen t =
		let toString team' =
			match team' with
				Korea->"Korea" | France->"France" | Usa->"Usa"
			| Brazil->"Brazil" | Japan->"Japan" | Nigeria->"Nigeria"
			| Cameroon->"Cameroon" | Poland->"Poland" | Portugal->"Portugal"
			| Italy->"Italy" | Germany->"Germany" | Sweden->"Sweden"
			| England->"England" | Croatia->"Croatia" | Argentina->"Argentina"
		in
		match t with
			LEAF team' -> toString team'
		| NODE (t1, t2) -> "("^(toParen t1)^" "^(toParen t2)^")"
	in
	let rec getTree t' =
		match t' with
			LEAF team' -> LEAF team'
		| NODE (t1, t2)
			-> if t1 = (LEAF team) then (getTree t2)
				 else if t2 = (LEAF team) then (getTree t1)
				 else NODE((getTree t1), (getTree t2))
	in
	
	if (getTree tourna)=(LEAF team) then ""
	else toParen(getTree tourna)