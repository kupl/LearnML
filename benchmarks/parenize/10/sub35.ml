type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		  | Poland | Portugal | Italy | Germany | Sweden | England
		  | Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

let rec parenize tree =
	let team_to_str t =
		match t with
		  Korea -> "Korea"| France -> "France"| Usa -> "Usa" | Brazil -> "Brazil"
		| Japan -> "Japan" | Nigeria -> "Nigeria" | Cameroon -> "Cameroon" 
		| Poland -> "Poland" | Portugal -> "Portugal" | Italy -> "Italy"
		| Germany -> "Germany" | Sweden -> "Sweden" | England -> "England" 
		| Croatia -> "Croatia" | Argentina -> "Argentina" 
	in
	match tree with
	  LEAF (team) -> team_to_str team
	| NODE (t1, t2) -> "(" ^ (parenize t1) ^ " " ^ (parenize t2) ^ ")"
