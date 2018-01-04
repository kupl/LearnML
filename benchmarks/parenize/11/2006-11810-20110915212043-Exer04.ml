type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	  | Poland | Portugal | Italy | Germany | Sweden | England
	  | Croatia | Argentina

type tourna = LEAF of team 
	    | NODE of tourna * tourna
	
exception Error of tourna

let rec parenize (tourna)  =
	match tourna with
	|LEAF (t) -> 
		(match t with
		|Korea -> "Korea"
		|France -> "France"
		|Usa -> "Usa"
		|Brazil -> "Brazil"
		|Japan -> "Japan"
		|Nigeria -> "Nigeria"
		|Cameroon -> "Cameroon"
		|Poland -> "Poland"
		|Portugal -> "Portugal"
		|Italy -> "Italy"
		|Germany -> "Germany"
		|Sweden -> "Sweden"
		|England -> "England"
		|Croatia -> "Croatia"
		|Argentina -> "Argentina"
		)
	|NODE (tourna1, tourna2) ->
		"(" ^ parenize (tourna1) ^ " " ^ parenize (tourna2) ^ ")"



