(*2006-11681 °­Çö¼®*)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		  | Poland | Portugal | Italy | Germany | Sweden | England
		  | Croatia | Argentina

type tourna = LEAF of team
			| NODE of tourna * tourna

let rec parenize t =
	match t with
	LEAF n -> (match n with
				Korea -> "Korea" 
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
				| Argentina -> "Argentina")
	| NODE(l,r) -> "("^(parenize l)^" "^(parenize r)^")"


