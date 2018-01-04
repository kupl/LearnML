(* ex5 *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Norway | Sweden | England | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let rec parenize t = 
	let mkstr team = 
		match team with
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
		| Norway -> "Norway"
		| Sweden -> "Sweden"
		| England -> "England"
		| Argentina -> "Argentina"
	in
	match t with
	  LEAF (team) -> (mkstr team)
	| NODE (t1, t2) -> "("^(parenize t1)^" "^(parenize t2)^")"
	