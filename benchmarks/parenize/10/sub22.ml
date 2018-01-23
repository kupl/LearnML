
(*Ex4*)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland
| Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

let name a = match a with Korea -> "Korea"
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
						| _ -> "There is no matching team"


let rec parenize : tourna -> string = fun tour ->
	match tour with LEAF t -> name(t)
				|	NODE (lef, rig) -> "("^(parenize lef)^" "^(parenize rig)^")"
