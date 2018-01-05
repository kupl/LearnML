type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

let nation = [(Korea,"Korea");(France,"France");(Usa,"Usa");(Brazil,"Brazil");(Japan,"Japan");(Nigeria,"Nigeria");(Cameroon,"cameroon");(Poland,"Poland");(Portugal,"Portugal");(Italy,"Italy");(Germany,"Germany");(Sweden,"Sweden");(England,"England");(Croatia,"Croatia");(Argentina,"Argentina")]

let rec parenize a =
		match a with
		NODE (b, c) -> "("^(parenize b)^" "^(parenize c)^")"
		| LEAF b -> List.assoc b nation

