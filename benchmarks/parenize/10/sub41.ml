(* 4190.310 Programming Language 			*
 * Homework #1 - Exercise 4 (대진표 스트링)	*
 * 2008-11744 Jongwook Choi 				*)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		  | Poland | Portugal | Italy | Germany | Sweden | England
		  | Croatia | Argentina

type tourna = LEAF of team | NODE of tourna * tourna


let rec parenize x = 
	let toString t = match t with
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
		| England -> "Englan"
		| Croatia -> "Croatia"
		| Argentina -> "Argentina"
	in
	match x with
	  LEAF t -> toString(t)
	| NODE (l, r) -> "(" ^ parenize(l) ^ " " ^ parenize(r) ^ ")"

