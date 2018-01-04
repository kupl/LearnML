(* 컴퓨터공학부 / 2005-11721 / 김재경 / 숙제1-4 *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland
          | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna
let name team =
    match team with
      Korea -> "Korea" | France -> "France" | Usa -> "Usa" | Brazil -> "Brazil"
    | Japan -> "Japan" | Nigeria -> "Nigeria" | Cameroon -> "Cameroon"
    | Poland -> "Poland" | Portugal -> "Portugal" | Italy -> "Italy" | Germany -> "Germany"
    | Sweden -> "Sweden" | England -> "England" | Croatia -> "Croatia" | Argentina -> "Argentina"
let rec parenize tourna =
      match tourna with
        NODE(t1,t2) -> "(" ^ parenize(t1) ^" "^ parenize(t2) ^ ")"
      | LEAF(team) -> name(team)