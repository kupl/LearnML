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
	| Sweden
	| England
	| Croatia
	| Argentina

type tourna = LEAF of team
	| NODE of tourna * tourna

let rec parenize : tourna -> string = fun tourna ->
	match tourna with
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
	| LEAF Sweden -> "Sweden"
	| LEAF England -> "Englang"
	| LEAF Croatia -> "Croatia"
	| LEAF Argentina -> "Argentina"
	| NODE (lt, rt) ->
		"(" ^ parenize lt ^ " " ^ parenize rt ^ ")"