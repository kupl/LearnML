(* ex1 tornament tree to string *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team | NODE of tourna * tourna

let rec toParen tm =
	let rec toString teamName =
		match teamName with
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
			| Argentina -> "Argentina" in
	let foldnode ( str1, str2 ) = "("^ str1 ^ " " ^ str2 ^ ")" in
	match tm with
		LEAF team -> toString team 
		| NODE ( tm1, tm2 ) -> foldnode( ( toParen tm1 ), ( toParen tm2 ) )
