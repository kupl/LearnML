type team 	= Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		| Poland | Portugal | Italy | Germany | Sweden | England
		| Croatia | Argentina
type tourna 	= LEAF of team
		| NODE of tourna * tourna


let rec parenize t =
	let
		team_to_string input_team =
			match input_team with
			| Korea       -> "Korea" 
			| France      -> "France"   
			| Usa	      -> "Usa"      
			| Brazil      -> "Brazil"   
			| Japan	      -> "Japan"    
			| Nigeria     -> "Nigeria"  
			| Cameroon    -> "Cameroon" 
			| Poland      -> "Poland"   
			| Portugal    -> "Portugal" 
			| Italy	      -> "Italy"    
			| Germany     -> "Germany"  
			| Sweden      -> "Sweden"   
			| England     -> "England"  
			| Croatia     -> "Croatia"  
			| Argentina   -> "Argentina"
	in
		match t with
		| LEAF(team1)	->	team_to_string(team1)
		| NODE(tourna1, tourna2)	->	"("^parenize(tourna1)^" "^parenize(tourna2)^")"