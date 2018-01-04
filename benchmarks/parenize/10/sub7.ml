(* CSE/ 2004-11920 / Yeseong Kim/ Prob 4*)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let rec parenize t =
		let name _t =
			match _t with
				Korea -> "Korea"
			|	France -> "France"
			|	Usa -> "Usa"
			|	Brazil -> "Brazil"
			|	Japan -> "Japan"
			|	Nigeria -> "Nigeria"
			|	Cameroon -> "Cameroon"
			| 	Poland -> "Poland"
			| 	Portugal -> "Portugal"
			| 	Italy -> "Italy"
			|	Germany -> "Germany"
			| 	Sweden -> "Sweden"
			| 	England -> "England"
			| 	Croatia -> "Croatia"
			|	Argentina -> "Argentina"

		in
		match t with
			NODE (t1, t2) -> (String.concat "" ["("; (parenize t1); " "; (parenize t2); ")"])
		|	LEAF (v) -> (name v)
