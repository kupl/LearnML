type team = Korea 
	| France 
	| Usa 
	| Brazil 
	| Japan 
	| Nigeria 
	| Cameroon
	| Poland 
	| Portugal 
	| Italy 
	| Germany 
	| Norway 
	| Sweden 
	| England
	| Argentina
type tourna = LEAF of team
	| NODE of tourna * tourna

let rec parenize tour =
	match tour with
	| LEAF Korea -> "Korea"
	| LEAF France -> "France"
	| LEAF Usa -> "Usa"
	| LEAF Brazil -> "Brazil"
	| LEAF Japan -> "Japan"
	| LEAF Nigeria -> "Nigeria"
	| LEAF Cameroon -> "Cameroon"
	| LEAF Poland -> "Poland"
	| LEAF Portugal -> "Portugal"
	| LEAF Italy -> "Italy"
	| LEAF Germany -> "Germany"
	| LEAF Norway -> "Norway"
	| LEAF Sweden -> "Sweden"
	| LEAF England -> "England"
	| LEAF Argentina -> "Argentina"
	| NODE (t1, t2) -> "(" ^ (parenize t1) ^ " " ^ (parenize t2) ^ ")"
