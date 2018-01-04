(*
2008-12155
±èÂùÈ£
*)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina

type tourna = LEAF of team
	| NODE of tourna * tourna

let rec parenize t =
	match t with 
	| LEAF k -> (match k with
		| Korea -> "Korea" | France -> "France" | Usa -> "Usa"
		| Brazil -> "Brazil" | Japan -> "Japan" | Nigeria -> "Nigeria"
		| Cameroon -> "Cameroon" | Poland -> "Poland" | Portugal -> "Portugal"
		| Italy -> "Italy" | Germany -> "Germany" | Sweden -> "Sweden"
		| England -> "England" | Croatia -> "Croatia" | Argentina -> "Argentina")
	| NODE(x, y) -> "(" ^ (parenize x) ^ " " ^ (parenize y) ^ ")"
