type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna


let rec drop = fun (t, nat) -> let rec toParen t1 = 
			
			let toStr t1 = match t1 with
  			Korea -> "Korea"
  			|France -> "France"
  			|Usa -> "Usa"
  			|Brazil -> "Brazil"
  			|Japan -> "Japan"
  			|Nigeria -> "Nigeria"
  			|Cameroon -> "Cameroon"
  			|Poland -> "Poland"
  			|Italy -> "Italy"
  			|Portugal -> "Portugal"
  			|Germany -> "Germany"
  			|Sweden -> "Sweden"
  			|England -> "England"
  			|Croatia -> "Croatia"
  			|Argentina -> "Argentina" in
			
			match t1 with
  			(NODE (a,b)) ->"("^(toParen a)^" "^(toParen b)^")"  			
			|(LEAF a) -> (toStr a) in
 
				let rec check a nat = match a with
						LEAF b -> if b = nat then true else false
						|NODE (a, b) -> if check a nat & check b nat then true else false in
						
				
				let rec temp = fun (t, nat) -> match t with 
  						NODE(a, b) -> if (check a nat = true) then temp (b, nat) else if (check b nat = true) then temp (a,nat) else (NODE(temp (a, nat),temp (b, nat))) 
						|(LEAF a) -> (LEAF a) in
		
			
			if (check t nat = true) then "" else toParen(temp(t,nat));;

let a = (NODE(NODE(LEAF Korea, LEAF Korea), LEAF Korea));;
let b = NODE(LEAF Korea, LEAF Korea);;
let c = NODE(NODE(LEAF Korea, LEAF Korea), NODE(LEAF Usa, LEAF Korea));;