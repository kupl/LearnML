
(* 2008-11720 ���ܸ� *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		| Poland | Portugal | Italy | Germany | Sweden | England
		| Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

let rec parenize tour = 
	match tour with
	LEAF Korea -> "Korea"
	| LEAF France -> "France"
	| LEAF Usa -> "Usa"
	| LEAF Brazil -> "Brazil"
	| LEAF Japan -> "Japan"
	| LEAF Nigeria -> "Nigeria"
	| LEAF Cameroon -> "Cameroon"
	| LEAF Poland -> "Poland"
	| LEAF Portugal -> "portugal"
	| LEAF Italy -> "Italy"
	| LEAF Germany -> "Germany"
	| LEAF Sweden -> "Sweden"
	| LEAF England -> "England"
	| LEAF Croatia -> "Croatia"
	| LEAF Argentina -> "Argentina"
	| NODE (t1, t2) -> "("^(parenize t1)^" "^(parenize t2)^")"
