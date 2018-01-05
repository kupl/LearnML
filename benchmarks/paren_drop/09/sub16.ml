type team = Korea | France | Usa | Brazil | Japan | Nigerea | Cameroon | Poland | Portugal
                  | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna


let rec drop (t, team) = 
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
		   | (NODE (t1, t2)) -> "("^(toParen t1)^" "^(toParen t2)^")" in
	match t with NODE(LEAF l1, LEAF l2) -> if l1 = team then (if l2 = team then "" else toParen (LEAF l2))
						else (if l2 = team then toParen (LEAF l1) else toParen t)
		   | NODE(LEAF l1, t2) -> let x = drop(t2, team) in
					  if l1 = team then x
					  else (if x = "" then (toParen (LEAF l1))
						else "("^(toParen (LEAF l1))^" "^x^")")
		   | NODE(t1, LEAF l2) -> let x = drop(t1, team) in
					  if l2 = team then x
					  else (if x = "" then (toParen (LEAF l2))
						else "("^x^" "^(toParen (LEAF l2))^")")
		   | NODE(t1, t2) -> let x = drop(t1, team) in
				     let y = drop(t2, team) in
				     if x = "" then (if y = "" then "" else y)
				     else (if y = "" then x else "("^x^" "^y^")")
		   | LEAF l -> if l = team then ""
				else toParen (LEAF l)





