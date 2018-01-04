(* Ex5 *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina
type tourna = LEAF of team
            | NODE of tourna * tourna

let rec parenize tourna =
   match tourna with
   | LEAF tm -> if tm = Korea then "Korea"				  
                else if tm = France then "France"
				else if tm = Usa then "Usa"
				else if tm = Brazil then "Brazil"
				else if tm = Japan then "Japan"
				else if tm = Nigeria then "Nigeria"
				else if tm = Cameroon then "Cameroon"
				else if tm = Poland then "Poland"
				else if tm = Portugal then "Portugal"
				else if tm = Italy then "Italy"
				else if tm = Germany then "Germany"
				else if tm = Norway then "Norway"
				else if tm = Sweden then "Sweden"
				else if tm = England then "England"
				else "Argentina"
   | NODE(tourna1, tourna2) -> "(" ^ parenize tourna1 ^ " " ^ parenize tourna2 ^ ")"