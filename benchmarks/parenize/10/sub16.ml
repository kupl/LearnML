type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Sweden | England
          | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let teamname t = match t with
    Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Sweden -> "Sweden"
  | England -> "England"
  | Croatia -> "Croatia"
  | Argentina -> "Argentina"

let rec parenize tr = match tr with
    LEAF l -> teamname l
  | NODE (tr1, tr2) -> "(" ^ (parenize tr1) ^ " " ^ (parenize tr2) ^ ")"