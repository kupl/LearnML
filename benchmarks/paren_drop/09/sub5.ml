type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
			| Poland | Portugal | Italy | Germany | Sweden | England
			| Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

let rec toParen a =
	match a with
	NODE(x, y) -> "(" ^ (toParen x) ^ " " ^ (toParen y) ^ ")"
	|LEAF Korea -> "Korea"
	|LEAF France -> "France"
	|LEAF Usa -> "Usa"
	|LEAF Brazil -> "Brazil"
	|LEAF Japan -> "Japan"
	|LEAF Nigeria -> "Nigeria"
	|LEAF Cameroon -> "Cameroon"
	|LEAF Poland -> "Poland"
	|LEAF Portugal -> "Portugal"
	|LEAF Italy -> "Italy"
	|LEAF Germany -> "Germany"
	|LEAF Sweden -> "Sweden"
	|LEAF England -> "England"
	|LEAF Croatia -> "Croatia"
	|LEAF Argentina -> "Argentina"

let rec drop(t, a) =
	match t with
	NODE(LEAF x, NODE (y,z)) ->
		if x = a then drop(NODE(y,z), a)
		else "(" ^ toParen(LEAF x) ^ " " ^ (drop(NODE (y,z), a)) ^ ")"
	|NODE(NODE (x,z), LEAF y) ->
		if y = a then drop(NODE (x,z), a)
		else "(" ^ (drop(NODE (x,z), a)) ^ " " ^ toParen(LEAF y) ^ ")"
	|NODE(LEAF x, LEAF y) ->
		if x = a && y = a then ""
		else if x = a then toParen(LEAF y)
		else if y = a then toParen(LEAF x)
		else "(" ^ toParen(LEAF x) ^ " " ^ toParen(LEAF y) ^ ")"
	|NODE(NODE (x,z), NODE (y,b)) ->
		"(" ^ (drop(NODE (x,z), a)) ^ " " ^ (drop(NODE (y,b), a)) ^ ")"
	|LEAF Korea -> "Korea"
	|LEAF France -> "France"
	|LEAF Usa -> "Usa"
	|LEAF Brazil -> "Brazil"
	|LEAF Japan -> "Japan"
	|LEAF Nigeria -> "Nigeria"
	|LEAF Cameroon -> "Cameroon"
	|LEAF Poland -> "Poland"
	|LEAF Portugal -> "Portugal"
	|LEAF Italy -> "Italy"
	|LEAF Germany -> "Germany"
	|LEAF Sweden -> "Sweden"
	|LEAF England -> "England"
	|LEAF Croatia -> "Croatia"
	|LEAF Argentina -> "Argentina"
