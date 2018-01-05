(* ex2 drop *)
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

let rec drop ( tnm, tm ) =
	match tnm with
		LEAF team -> if team = tm then "" else toParen tnm
		| NODE ( tm1, tm2 ) ->
			( let d1 = drop ( tm1, tm ) in
			  let d2 = drop ( tm2, tm ) in
		      if d1 = "" then d2
			  else if d2 = "" then d1
			  else "(" ^ d1 ^ " " ^ d2 ^  ")" )

