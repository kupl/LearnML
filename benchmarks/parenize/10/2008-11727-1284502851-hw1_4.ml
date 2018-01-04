type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina


type tourna = LEAF of team
 	    | NODE of tourna * tourna


let nations = [(Korea,"Korea");(France,"France");(Usa,"Usa");(Brazil,"Brazil");(Japan,"Japan");(Nigeria,"Nigeria");(Cameroon,"Cameroon");(Poland,"Poland");(Portugal,"Portugal");(Italy,"Italy");(Germany,"Germany");(Sweden,"Sweden");(England,"England");(Croatia,"Croatia");(Argentina,"Argentina")];;




let last pair =
	match pair with
	(a,b) -> b;;

let f k pair =
	match pair with
	(a,b) -> a=k;;


let rec parenize t =
	match t with
	LEAF a -> last (List.hd (List.filter (f a) nations))
	| NODE (a,b) -> "("^(parenize a)^" "^(parenize b)^")";;

