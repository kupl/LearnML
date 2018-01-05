(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_second_homework\HW2_Exercise2.ml *)

(*Exercise 2*)
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

let rec drop (a,b) =
		let rec drop_h (x,y) =
		let temp = (LEAF y) in	
		match x with
		  NODE (left, right) -> (
					if temp=right then left
					else if temp=left then right
					else NODE (drop_h (left,y) , (drop_h (right,y)))
					) 
		| LEAF c -> LEAF c
		in
			match a with
				LEAF t ->(
					   if t=b then ""
					   else "Ilegal input"
					)
				|_-> toParen (drop_h (a,b))
 			
  ;;



