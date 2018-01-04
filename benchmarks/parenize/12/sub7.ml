

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
			| Poland | Portugal | Italy | Germany | Norway | Sweden | England
			| Argentina;;
type tourna = LEAF of team
			| NODE of tourna * tourna;;

let rec parenize input =
	let string_of_team tm =
		match tm with
		|	Korea -> "Korea"
		|	France -> "France"
		|	Usa -> "Usa"
		|	Brazil -> "Brazil"
		|	Japan -> "Japan"
		|	Nigeria -> "Nigeria"
		|	Cameroon -> "Cameroon"
		|	Poland -> "Poland"
		|	Portugal -> "Portugal"
		|	Italy -> "Italy"
		|	Germany -> "Germany"
		|	Norway -> "Norway"
		|	Sweden -> "Sweden"
		|	England -> "England"
		|	Argentina -> "Argentina"
	in
	match input with
	|	LEAF lf -> string_of_team lf
	|	NODE (tourna1, tourna2) -> Printf.sprintf "(%s %s)" (parenize tourna1) (parenize tourna2)
	;;
