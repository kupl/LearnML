exception Error of string;;

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina;;

type tourna = LEAF of team | NODE of tourna * tourna;;

let rec parenize_list t = match t with 
    LEAF l -> (match l with 
		   Korea -> ["Korea"]
		 | France -> ["France"]
		 | Usa -> ["Usa"]
		 | Brazil -> ["Brazil"]
		 | Japan -> ["Japan"]
		 | Nigeria -> ["Nigeria"]
		 | Cameroon -> ["Cameroon"]
		 | Poland -> ["Poland"]
		 | Portugal -> ["Portugal"]
		 | Italy -> ["Italy"]
		 | Germany -> ["Germany"]
		 | Sweden -> ["Sweden"]
		 | England -> ["England"]
		 | Croatia -> ["Croatia"]
		 | Argentina -> ["Argentina"])
  | NODE (t1, t2) -> (List.append (List.append (List.append (List.append ["("] (parenize_list t1)) [" "] ) (parenize_list t2)) [")"]);;
		 
let parenize t = (String.concat "" (parenize_list t));;
