exception ERROR

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon 
			| Poland | Portugal | Italy | Germany | Sweden | England
			| Croatia | Argentina

type tourna = LEAF of team
			| NODE of tourna * tourna

let rec drop (tour, tm) =  

let rec toParen x =
		match x with
		(NODE(a, b)) -> "(" ^ toParen(a) ^ " " ^ toParen(b) ^ ")"
		|(LEAF Korea) -> "Korea"
		|(LEAF France) -> "France"
		|(LEAF Usa) -> "Usa"
		|(LEAF Brazil) -> "Brazil"
		|(LEAF Japan) -> "Japan"
		|(LEAF Nigeria) -> "Nigeria"
		|(LEAF Cameroon) -> "Cameroon"
		|(LEAF Poland) -> "Poland"
		|(LEAF Portugal) -> "Portugal"
		|(LEAF Italy) -> "Italy"
		|(LEAF Germany) -> "Germany"
		|(LEAF Sweden) -> "Sweden"
		|(LEAF England) -> "England"
		|(LEAF Croatia) -> "Croatia"
		|(LEAF Argentina) -> "Argentina"
in

		match (tour, tm) with
		(LEAF a, b) -> 
			if a=b then raise ERROR
			else "(" ^ toParen(LEAF a) ^ ")"
		|(NODE(LEAF a, LEAF b), c) -> if a=c && b=c then raise ERROR
											else if a=c then toParen(LEAF b) 
											else if b=c then toParen(LEAF a) 
											else "("^toParen(LEAF a)^" "^toParen(LEAF b)^")"
		|(NODE(NODE(a, b), LEAF c), d) -> if c=d then toParen(NODE(a, b))
											else "("^drop(NODE(a, b), d)^" "^toParen(LEAF c)^")"
		|(NODE(LEAF c, NODE(a, b)), d) -> if c=d then toParen(NODE(a, b))
											else "("^toParen(LEAF c)^" "^drop(NODE(a, b), d)^")"
		|(NODE(NODE(a, b), NODE(c, d)), e) -> "("^drop(NODE(a, b), e)^" "^drop(NODE(c, d), e)^")"


