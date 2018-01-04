type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		| Poland | Portugal | Italy | Germany | Sweden | England
		| Croatia | Argentina
type tourna = LEAF of team
		| NODE of tourna * tourna


let rec toParen tn =
	let tostring t =
		match t with
			| Korea -> "Korea" | France -> "France" | Usa -> "Usa" | Brazil -> "Brazil"
			| Japan -> "Japan" | Nigeria -> "Nigeria" | Cameroon -> "Cameroon" | Poland -> "Poland"
			| Portugal -> "Portugal" | Italy -> "Italy" | Germany -> "Germany" | Sweden -> "Sweden" | England -> "England"
			| Croatia -> "Croatia" | Argentina -> "Argentina" in
	match tn with
		| LEAF t -> tostring( t )
		| NODE( a, b ) -> "(" ^ ( toParen( a ) ) ^ " " ^ ( toParen( b ) ) ^")";;


(*print_string ( toParen(NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)) );;*)