(* 2006-11782 Song Young-chan, Hw2-1 tournament *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland 
          | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team
	    | NODE of tourna * tourna

let rec toParen (input_tourna) = 
	match input_tourna with
	  LEAF(Korea) -> "Korea"
	| LEAF(France) -> "France"
	| LEAF(Usa) -> "Use"
	| LEAF(Brazil) -> "Brazil"
	| LEAF(Japan) -> "Japan"
	| LEAF(Nigeria) -> "Nigeria"
	| LEAF(Cameroon) -> "Cameroon"
	| LEAF(Poland) -> "Poland"
	| LEAF(Portugal) -> "Portugal"
	| LEAF(Italy) -> "Italy"
	| LEAF(Germany) -> "Germany"
	| LEAF(Sweden) -> "Sweden"
	| LEAF(England) -> "England"
	| LEAF(Croatia) -> "Croatia"
	| LEAF(Argentina) -> "Argentina"
	| NODE(left, right) -> "("^(toParen left)^" "^(toParen right)^")"
