type team =
  | Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland
  | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina;;

type tourna = LEAF of team | NODE of tourna * tourna;;

let rec parenize tourna =
  let string_of_team t =
    match t with
    | Korea -> "Korea"
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
  in
  match tourna with
  | LEAF t -> string_of_team t
  | NODE (tl, tr) -> "(" ^ parenize tl ^ " " ^ parenize tr ^ ")";;
