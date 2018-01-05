type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
  | Poland | Portugal | Italy | Germany | Sweden | England
  | Croatia | Argentina

type tourna = LEAF of team
  | NODE of tourna * tourna

let print_team team =
  match team with Korea -> print_string "Korea"
  | France -> print_string "France"
  | Usa -> print_string "Usa"
  | Brazil -> print_string "Brazil"
  | Japan -> print_string "Japan"
  | Nigeria -> print_string "Nigeria"
  | Cameroon -> print_string "Cameroon"
  | Poland -> print_string "Poland"
  | Portugal -> print_string "Portugal"
  | Italy -> print_string "Italy"
  | Germany -> print_string "Germany"
  | Sweden -> print_string "Sweden"
  | England -> print_string "England"
  | Croatia -> print_string "Croatia"
  | Argentina -> print_string "Argentina"

let rec parenize t =
  match t with LEAF l -> print_team l
  | NODE(a, b) -> print_string "("; parenize a; print_string " "; parenize b; print_string ")";;

let _ = parenize(NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil));;