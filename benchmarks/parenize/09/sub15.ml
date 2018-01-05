type team = Korea | France | Usa | Brazil | Japan | Nigerea | Cameroon | Poland | Portugal
                  | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let rec toParen t = 
	match t with (LEAF l) -> (match l with Korea -> "Korea"
						| France -> "France"
					      	| Usa -> "Usa"
						| Brazil -> "Brazil"
						| Japan -> "Japan"
						| Nigerea -> "Nigerea"
						| Cameroon -> "Cameroon"
						| Poland -> "Poland"
						| Portugal -> "Portugal"
						| Italy -> "Italy"
						| Germany -> "Germany"
						| Sweden -> "Sweden"
						| England -> "England"
						| Croatia -> "Croatia"
						| Argentina -> "Argentina" )
		   | (NODE (t1, t2)) -> "("^(toParen t1)^" "^(toParen t2)^")"

		