type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team
	    | NODE of tourna * tourna


let rec parenize tour = 
 (* team : team -> string ; team을 string으로 변환해주는 테이블 *)
 let teamToString t =
  match t with
   Korea -> "Korea"
   |France -> "France"
   |Usa -> "Usa"
   |Brazil -> "Brazil"
   |Japan -> "Japan"
   |Nigeria -> "Nigeria"
   |Cameroon -> "Cameroon"
   |Poland -> "Poland"
   |Portugal -> "Portugal"
   |Italy -> "Italy"
   |Germany -> "Germany"
   |Sweden -> "Sweden"
   |England -> "England"
   |Croatia -> "Croatia"
   |Argentina -> "Argentina"
 in
 
 (* 함수 본문 *)
 match tour with
  LEAF t -> (teamToString t)
  |NODE (t1, t2) -> ("(" ^ (parenize t1) ^ " " ^ (parenize t2) ^ ")")
;;


(* test code *)
let kr = Korea;;
let fr = France;;
let usa = Usa;;
let br = Brazil;;
let jp = Japan;;

