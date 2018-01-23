
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
			| Poland | Portugal | Italy | Germany | Sweden | England
			| Croatia | Argentina

type tourna = LEAF of team
			| NODE of tourna * tourna

			
let rec toParen x  =
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