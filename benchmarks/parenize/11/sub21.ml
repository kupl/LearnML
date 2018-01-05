
(* ex 4 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		  | Poland | Portugal | Italy | Germany | Sweden | England
		  | Croatia | Argentina

type tourna = LEAF of team
			| NODE of tourna * tourna

let rec parenize t = 
	match t with	
	| NODE (t1,t2) -> "(" ^ (parenize t1) ^ " " ^ (parenize t2) ^ ")" 
	| LEAF t3 -> match t3 with
			| Korea -> "Korea" | France -> "France" | Usa -> "Usa"
			| Brazil -> "Brazil" | Japan -> "Japan" 
			| Nigeria -> "Nigeria" | Cameroon -> "Cameroon"
			| Poland -> "Poland" | Portugal -> "Portugal"
			| Italy -> "Italy" | Germany -> "Germany"
			| Sweden -> "Sweden" | England -> "England"
			| Croatia -> "Croatia" | Argentina -> "Argentina"


(* parenize(NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)) *)
