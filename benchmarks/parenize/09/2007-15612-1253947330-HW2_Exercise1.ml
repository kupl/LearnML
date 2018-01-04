(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_second_homework\HW2_Exercise1.ml *)

(*Exercise 1*)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		| Poland | Portugal | Italy | Germany | Sweden | England
		| Croatia | Argentina

type tourna = LEAF of team
		| NODE of tourna*tourna

let rec toParen e =
	let rec stringf tea =
	match tea with
		| Korea -> "Korea"
		| France -> "France"
		| Usa -> "Usa"
		| Brazil -> "Brazil"
		| Japan -> "Japan"
		| Nigeria -> "Nigeria"
		| Cameroon -> "Cameroon"
		| Poland -> "poland"
		| Portugal -> "Portugal"
		| Italy -> "Italy" 
		| Germany -> "Germany" 
		| Sweden -> "Sweden"
		| England -> "England" 
		| Croatia -> "Croatia"
		| Argentina -> "Argentina"
						in		 
  			match e with
			| NODE (left, right) -> "(" ^ (toParen left) ^ " " ^ (toParen right) ^ ")"
			| LEAF x -> stringf x 
  						;;

