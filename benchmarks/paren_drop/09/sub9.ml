(*2006-11720 2-2 KimEunSol*)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland
		| Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna
let rec toParen(a) = 
	match a with NODE (a, b) -> "(" ^ toParen(a) ^ " " ^ toParen(b) ^ ")"
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

let rec drop(a, b) =
	match a with LEAF x -> (
							if x = b then "" 
							else "("^toParen(LEAF x)^")"
							)
	|NODE(LEAF x, LEAF y) ->(
							if x=b && y = b then ""
							else if x = b then toParen(LEAF y)
							else if y = b then toParen(LEAF x)
							else "(" ^ toParen(LEAF x) ^ " " ^ toParen(LEAF y) ^ ")"
							)
	|NODE(NODE(x, y), LEAF z) -> (
							if z = b then toParen(NODE(x, y))
							else "(" ^ drop(NODE(x, y), b) ^ " " ^ toParen(LEAF z) ^ ")"
							)
	|NODE(LEAF z, NODE(x, y)) -> (
							if z = b then toParen(NODE(x, y))
							else "(" ^ toParen(LEAF z) ^ " " ^ drop(NODE(x, y), b) ^ ")"
							)
	|NODE(NODE(x,y), NODE(z,w)) -> "(" ^ drop(NODE(x,y), b) ^ " " ^ drop(NODE(z, w), b) ^ ")" 
